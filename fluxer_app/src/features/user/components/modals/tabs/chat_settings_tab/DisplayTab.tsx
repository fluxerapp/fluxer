// SPDX-License-Identifier: AGPL-3.0-or-later

import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import {PRODUCT_NAME} from '@app/features/app/config/I18nDisplayConstants';
import {REACTIONS_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {formatPermissionLabel} from '@app/features/permissions/utils/PermissionUtils';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import {SwitchGroup, SwitchGroupItem} from '@app/features/ui/components/SwitchGroup';
import type {RadioOption} from '@app/features/ui/radio_group/RadioGroup';
import {RadioGroup} from '@app/features/ui/radio_group/RadioGroup';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/chat_settings_tab/DisplayTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {RenderSpoilers} from '@fluxer/constants/src/UserConstants';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';

const ON_CLICK_DESCRIPTOR = msg({
	message: 'On click',
	comment: 'Short label in the display tab. Keep it concise.',
});
const SHOW_SPOILER_CONTENT_WHEN_CLICKED_DESCRIPTOR = msg({
	message: 'Show spoiler content when clicked',
	comment: 'Label in the display tab.',
});
const IN_CHANNELS_I_MODERATE_DESCRIPTOR = msg({
	message: 'In channels I moderate',
	comment: 'Label in the display tab.',
});
const ALWAYS_SHOW_SPOILER_CONTENT_IN_CHANNELS_WHERE_YOU_DESCRIPTOR = msg({
	message: 'Always show spoiler content in channels where you have the "{manageMessagesPermissionLabel}" permission',
	comment:
		'Description text in the display tab. Preserve {manageMessagesPermissionLabel}; it is inserted by code. Keep the tone plain and specific.',
});
const ALWAYS_DESCRIPTOR = msg({
	message: 'Always',
	comment: 'Short label in the display tab. Keep it concise.',
});
const ALWAYS_SHOW_SPOILER_CONTENT_DESCRIPTOR = msg({
	message: 'Always show spoiler content',
	comment: 'Label in the display tab.',
});
const MEDIA_DISPLAY_DESCRIPTOR = msg({
	message: 'Media display',
	comment: 'Short label in the display tab. Keep it concise.',
});
const WHEN_POSTED_AS_LINKS_TO_CHAT_DESCRIPTOR = msg({
	message: 'When posted as links in chat',
	comment: 'Label in the display tab.',
});
const WHEN_UPLOADED_DIRECTLY_TO_DESCRIPTOR = msg({
	message: 'When uploaded directly to {productName}',
	comment: 'Label in the display tab. Preserve {productName}; it is inserted by code.',
});
const LINK_PREVIEWS_DESCRIPTOR = msg({
	message: 'Link previews',
	comment: 'Short label in the display tab. Keep it concise.',
});
const SHOW_EMBEDS_AND_PREVIEW_WEBSITE_LINKS_DESCRIPTOR = msg({
	message: 'Show embeds and preview website links',
	comment: 'Label in the display tab.',
});
const SHOW_EMOJI_REACTIONS_ON_MESSAGES_DESCRIPTOR = msg({
	message: 'Show emoji reactions on messages',
	comment: 'Label in the display tab.',
});
const SPOILER_CONTENT_DESCRIPTOR = msg({
	message: 'Spoiler content',
	comment: 'Short label in the display tab. Keep it concise.',
});
const spoilerOptions = (i18n: I18n): ReadonlyArray<RadioOption<number>> => {
	const manageMessagesPermissionLabel = formatPermissionLabel(i18n, Permissions.MANAGE_MESSAGES);
	return [
		{
			value: RenderSpoilers.ON_CLICK,
			name: i18n._(ON_CLICK_DESCRIPTOR),
			desc: i18n._(SHOW_SPOILER_CONTENT_WHEN_CLICKED_DESCRIPTOR),
		},
		{
			value: RenderSpoilers.IF_MODERATOR,
			name: i18n._(IN_CHANNELS_I_MODERATE_DESCRIPTOR),
			desc: i18n._(ALWAYS_SHOW_SPOILER_CONTENT_IN_CHANNELS_WHERE_YOU_DESCRIPTOR, {manageMessagesPermissionLabel}),
		},
		{
			value: RenderSpoilers.ALWAYS,
			name: i18n._(ALWAYS_DESCRIPTOR),
			desc: i18n._(ALWAYS_SHOW_SPOILER_CONTENT_DESCRIPTOR),
		},
	];
};
export const DisplayTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const userSettings = UserSettings;
	return (
		<>
			<SettingsTabSection
				title={i18n._(MEDIA_DISPLAY_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.display-tab.display-tab-content.settings-tab-section"
			>
				<div
					className={styles.sectionContent}
					data-flx="user.chat-settings-tab.display-tab.display-tab-content.section-content"
				>
					<SwitchGroup data-flx="user.chat-settings-tab.display-tab.display-tab-content.switch-group">
						<SwitchGroupItem
							label={i18n._(WHEN_POSTED_AS_LINKS_TO_CHAT_DESCRIPTOR)}
							value={userSettings.inlineEmbedMedia}
							onChange={(value) => UserSettingsCommands.update({inlineEmbedMedia: value})}
							data-flx="user.chat-settings-tab.display-tab.display-tab-content.switch-group-item.update"
						/>
						<SwitchGroupItem
							label={i18n._(WHEN_UPLOADED_DIRECTLY_TO_DESCRIPTOR, {productName: PRODUCT_NAME})}
							value={userSettings.inlineAttachmentMedia}
							onChange={(value) => UserSettingsCommands.update({inlineAttachmentMedia: value})}
							data-flx="user.chat-settings-tab.display-tab.display-tab-content.switch-group-item.update--2"
						/>
					</SwitchGroup>
				</div>
			</SettingsTabSection>
			<SettingsTabSection
				title={i18n._(LINK_PREVIEWS_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.display-tab.display-tab-content.settings-tab-section--2"
			>
				<div
					className={styles.sectionContent}
					data-flx="user.chat-settings-tab.display-tab.display-tab-content.section-content--2"
				>
					<Switch
						label={i18n._(SHOW_EMBEDS_AND_PREVIEW_WEBSITE_LINKS_DESCRIPTOR)}
						value={userSettings.renderEmbeds}
						onChange={(value) => UserSettingsCommands.update({renderEmbeds: value})}
						data-flx="user.chat-settings-tab.display-tab.display-tab-content.switch.update"
					/>
				</div>
			</SettingsTabSection>
			<SettingsTabSection
				title={i18n._(REACTIONS_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.display-tab.display-tab-content.settings-tab-section--3"
			>
				<div
					className={styles.sectionContent}
					data-flx="user.chat-settings-tab.display-tab.display-tab-content.section-content--3"
				>
					<Switch
						label={i18n._(SHOW_EMOJI_REACTIONS_ON_MESSAGES_DESCRIPTOR)}
						value={userSettings.renderReactions}
						onChange={(value) => UserSettingsCommands.update({renderReactions: value})}
						data-flx="user.chat-settings-tab.display-tab.display-tab-content.switch.update--2"
					/>
				</div>
			</SettingsTabSection>
			<SettingsTabSection
				title={i18n._(SPOILER_CONTENT_DESCRIPTOR)}
				data-flx="user.chat-settings-tab.display-tab.display-tab-content.settings-tab-section--4"
			>
				<div
					className={styles.radioSection}
					data-flx="user.chat-settings-tab.display-tab.display-tab-content.radio-section"
				>
					<RadioGroup
						options={spoilerOptions(i18n)}
						value={userSettings.renderSpoilers}
						onChange={(value) => UserSettingsCommands.update({renderSpoilers: value})}
						aria-label={i18n._(SPOILER_CONTENT_DESCRIPTOR)}
						data-flx="user.chat-settings-tab.display-tab.display-tab-content.radio-group.update"
					/>
				</div>
			</SettingsTabSection>
		</>
	);
});
