/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import {Trans, useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {MessagePreviewContext, MessageStates, MessageTypes} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import type {RadioOption} from '~/components/uikit/RadioGroup/RadioGroup';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import {Slider} from '~/components/uikit/Slider';
import {ChannelRecord} from '~/records/ChannelRecord';
import {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import UserStore from '~/stores/UserStore';
import {isNewMessageGroup} from '~/utils/MessageGroupingUtils';
import appearanceTabStyles from '../AppearanceTab.module.css';
import styles from './MessagesTab.module.css';

export const AppearanceTabPreview = observer(() => {
	const {t} = useLingui();
	const {messageDisplayCompact} = UserSettingsStore;
	const currentUser = UserStore.getCurrentUser();
	const author = currentUser?.toJSON() || {
		id: 'preview-user',
		username: 'PreviewUser',
		discriminator: '0000',
		global_name: 'Preview User',
		avatar: null,
		bot: false,
		system: false,
		flags: 0,
	};

	const fakeChannel = new ChannelRecord({
		id: 'fake-channel',
		type: 0,
		name: 'fake-channel',
		position: 0,
		parent_id: null,
		topic: null,
		url: null,
		nsfw: false,
		last_message_id: null,
		last_pin_timestamp: null,
		bitrate: null,
		user_limit: null,
		permission_overwrites: [],
	});

	const baseTime = new Date();
	const messageContents = [
		{content: t`This is how messages appear`, offsetMinutes: 0},
		{content: t`With different display modes available`, offsetMinutes: 1},
		{content: t`Customize the spacing and size`, offsetMinutes: 2},
		{content: t`Waiting for you to...`, offsetMinutes: 10},
		{content: t`... turn dense mode on. Nice!`, offsetMinutes: 11},
	];
	const fakeMessages = messageContents.map(({content, offsetMinutes}, index) => {
		const timestamp = new Date(baseTime.getTime() + offsetMinutes * 60 * 1000);
		return new MessageRecord(
			{
				id: `preview-${index + 1}`,
				channel_id: 'fake-channel',
				author,
				type: MessageTypes.DEFAULT,
				flags: 0,
				pinned: false,
				mention_everyone: false,
				content,
				timestamp: timestamp.toISOString(),
				state: MessageStates.SENT,
			},
			{skipUserCache: true},
		);
	});

	return (
		<div className={appearanceTabStyles.previewWrapper}>
			<div
				className={clsx(
					appearanceTabStyles.previewContainer,
					messageDisplayCompact
						? appearanceTabStyles.previewContainerCompact
						: appearanceTabStyles.previewContainerCozy,
				)}
			>
				<div className={appearanceTabStyles.previewMessagesContainer} key="appearance-messages-preview-scroller">
					{fakeMessages.map((message, index) => {
						const prevMessage = index > 0 ? fakeMessages[index - 1] : undefined;
						const isNewGroup = isNewMessageGroup(fakeChannel, prevMessage, message);
						const shouldGroup = !messageDisplayCompact && !isNewGroup;
						return (
							<Message
								key={message.id}
								channel={fakeChannel}
								message={message}
								prevMessage={prevMessage}
								previewContext={MessagePreviewContext.SETTINGS}
								shouldGroup={shouldGroup}
							/>
						);
					})}
				</div>
				<div className={appearanceTabStyles.previewOverlay} />
			</div>
		</div>
	);
});

export const MessagesTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const {messageDisplayCompact} = UserSettingsStore;
	const messageGroupSpacing = AccessibilityStore.messageGroupSpacing;
	const showUserAvatarsInCompactMode = AccessibilityStore.showUserAvatarsInCompactMode;
	const showMessageDividers = AccessibilityStore.showMessageDividers;
	const mobileLayout = MobileLayoutStore;

	const messageDisplayOptions: ReadonlyArray<RadioOption<boolean>> = [
		{value: false, name: t`Comfy`, desc: t`Spacious layout with clear visual separation between messages.`},
		{value: true, name: t`Dense`, desc: t`Maximizes visible messages with minimal spacing.`},
	];

	if (mobileLayout.enabled) {
		return <Trans>Message display settings are only available on desktop.</Trans>;
	}

	return (
		<>
			<RadioGroup
				options={messageDisplayOptions}
				value={messageDisplayCompact}
				onChange={(value) => {
					UserSettingsActionCreators.update({messageDisplayCompact: value});
				}}
				aria-label={t`Message display mode`}
			/>

			{messageDisplayCompact ? (
				<div className={styles.switchWrapper}>
					<Switch
						label={t`Hide user avatars`}
						value={!showUserAvatarsInCompactMode}
						onChange={(value) => AccessibilityActionCreators.update({showUserAvatarsInCompactMode: !value})}
					/>
				</div>
			) : null}

			<SettingsTabSection
				title={t`Space between message groups`}
				description={t`Adjust the spacing between groups of messages.`}
			>
				<Slider
					defaultValue={messageGroupSpacing}
					factoryDefaultValue={messageDisplayCompact ? 0 : 16}
					markers={[0, 4, 8, 16, 24]}
					stickToMarkers={true}
					onValueChange={(value) => AccessibilityActionCreators.update({messageGroupSpacing: value})}
					onMarkerRender={(value) => `${value}px`}
				/>
				<div className={styles.switchWrapper}>
					<Switch
						label={t`Show divider lines between message groups`}
						description={t`Display separator lines between different groups of messages for better visual distinction.`}
						value={showMessageDividers}
						onChange={(value) => AccessibilityActionCreators.update({showMessageDividers: value})}
					/>
				</div>
			</SettingsTabSection>
		</>
	);
});
