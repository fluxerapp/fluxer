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

import {useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import React from 'react';
import {MessagePreviewContext, MessageStates, MessageTypes, StatusTypes} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {SettingsSection} from '~/components/modals/shared/SettingsSection';
import {SettingsTabContainer, SettingsTabContent} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import {ChannelRecord} from '~/records/ChannelRecord';
import {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import UserStore from '~/stores/UserStore';
import {AnimationTabContent} from './AccessibilityTab/AnimationTab';
import {KeyboardTabContent} from './AccessibilityTab/KeyboardTab';
import {MotionTabContent} from './AccessibilityTab/MotionTab';
import {VisualTabContent} from './AccessibilityTab/VisualTab';
import styles from './AccessibilityTab.module.css';

export const AccessibilityTabPreview = observer(() => {
	const {t} = useLingui();
	const alwaysUnderlineLinks = AccessibilityStore.alwaysUnderlineLinks;

	const fakeData = React.useMemo(() => {
		const tabOpenedAt = new Date();
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
			id: 'fake-accessibility-channel',
			type: 0,
			name: 'accessibility-preview',
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

		const fakeMessage = new MessageRecord(
			{
				id: 'accessibility-preview-1',
				channel_id: 'fake-accessibility-channel',
				author,
				type: MessageTypes.DEFAULT,
				flags: 0,
				pinned: false,
				mention_everyone: false,
				content: t`This shows how links appear: https://fluxer.app`,
				timestamp: tabOpenedAt.toISOString(),
				state: MessageStates.SENT,
			},
			{skipUserCache: true},
		);

		return {fakeChannel, fakeMessage};
	}, []);

	return (
		<div className={styles.previewWrapper}>
			<div className={styles.previewContainer}>
				<div className={styles.previewActionsRow}>
					<Button small={true} onClick={() => {}}>
						{t`Preview Button`}
					</Button>
					<div className={styles.previewAvatarsRow}>
						<MockAvatar size={32} status={StatusTypes.ONLINE} />
						<MockAvatar size={32} status={StatusTypes.DND} />
						<MockAvatar size={32} status={StatusTypes.IDLE} />
					</div>
				</div>
				<div className={styles.previewMessageContainer}>
					<Message
						channel={fakeData.fakeChannel}
						message={fakeData.fakeMessage}
						previewContext={MessagePreviewContext.SETTINGS}
						previewOverrides={{
							usernameColor: '#e91e63',
							...(alwaysUnderlineLinks
								? {
										linkStyle: 'always-underline',
									}
								: {}),
						}}
					/>
				</div>
			</div>
		</div>
	);
});

const AccessibilityTabComponent: React.FC = observer(() => {
	const {t} = useLingui();
	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsSection
					id="visual"
					title={t`Visual`}
					description={t`Customize visual elements to improve visibility and readability.`}
				>
					<VisualTabContent />
				</SettingsSection>

				<SettingsSection id="keyboard" title={t`Keyboard`} description={t`Customize keyboard navigation behavior.`}>
					<KeyboardTabContent />
				</SettingsSection>

				<SettingsSection
					id="animation"
					title={t`Animation`}
					description={t`Control animated content throughout the app.`}
				>
					<AnimationTabContent />
				</SettingsSection>

				<SettingsSection
					id="motion"
					title={t`Motion`}
					description={t`Control animations and transitions throughout the app.`}
					isAdvanced
					defaultExpanded={false}
				>
					<MotionTabContent />
				</SettingsSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});

export default AccessibilityTabComponent;
