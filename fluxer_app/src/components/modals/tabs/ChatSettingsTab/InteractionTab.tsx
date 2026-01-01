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
import * as AccessibilityActionCreators from '~/actions/AccessibilityActionCreators';
import {MessageStates, MessageTypes} from '~/Constants';
import {Message} from '~/components/channel/Message';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {KeyboardKey} from '~/components/uikit/KeyboardKey';
import {SwitchGroup, SwitchGroupItem} from '~/components/uikit/SwitchGroup';
import {ChannelRecord} from '~/records/ChannelRecord';
import {MessageRecord} from '~/records/MessageRecord';
import AccessibilityStore from '~/stores/AccessibilityStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import UserStore from '~/stores/UserStore';
import {SHIFT_KEY_SYMBOL} from '~/utils/KeyboardUtils';
import styles from './InteractionTab.module.css';

const MessageActionBarPreview = observer(
	({
		showActionBar,
		showShiftExpand,
		onlyMoreButton,
	}: {
		showActionBar: boolean;
		showQuickReactions: boolean;
		showShiftExpand: boolean;
		onlyMoreButton: boolean;
	}) => {
		const {t} = useLingui();
		const fakeData = React.useMemo(() => {
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
				id: 'action-bar-fake-channel',
				type: 0,
				name: 'action-bar-fake-channel',
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
					id: 'action-bar-preview-message',
					channel_id: 'action-bar-fake-channel',
					author,
					type: MessageTypes.DEFAULT,
					flags: 0,
					pinned: false,
					mention_everyone: false,
					content: showActionBar ? t`This message shows the action bar` : t`This message doesn't show the action bar`,
					timestamp: new Date().toISOString(),
					state: MessageStates.SENT,
				},
				{skipUserCache: true},
			);

			return {fakeChannel, fakeMessage};
		}, [showActionBar]);

		return (
			<div className={styles.previewContainer}>
				<div
					className={styles.previewBox}
					data-force-show-action-bar={showActionBar ? 'true' : 'false'}
					style={{
						pointerEvents: 'none',
					}}
				>
					{showActionBar && (
						<style>
							{`
							[data-force-show-action-bar="true"] [class*="buttons"] {
								opacity: 1 !important;
								pointer-events: none !important;
							}
							[data-force-show-action-bar="true"] [class*="hoverAction"] {
								opacity: 1 !important;
								pointer-events: none !important;
							}
						`}
						</style>
					)}
					<Message
						channel={fakeData.fakeChannel}
						message={fakeData.fakeMessage}
						removeTopSpacing={true}
						previewMode={true}
					/>
				</div>
				<div
					className={`${styles.shiftHint} ${!showActionBar || onlyMoreButton || !showShiftExpand ? styles.shiftHintDisabled : ''}`}
				>
					<span className={styles.shiftHintText}>{t`Hold`}</span>
					<KeyboardKey>{SHIFT_KEY_SYMBOL}</KeyboardKey>
					<span className={styles.shiftHintText}>{t`to expand action bar`}</span>
				</div>
			</div>
		);
	},
);

export const InteractionTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const mobileLayout = MobileLayoutStore;

	const {
		showMessageActionBar,
		showMessageActionBarQuickReactions,
		showMessageActionBarShiftExpand,
		showMessageActionBarOnlyMoreButton,
	} = AccessibilityStore;

	if (mobileLayout.enabled) {
		return (
			<SettingsTabSection
				title="Message Action Bar"
				description="Message action bar settings are only available on desktop."
			>
				{null}
			</SettingsTabSection>
		);
	}

	return (
		<SettingsTabSection
			title="Message Action Bar"
			description="Customize the action bar that appears when hovering over messages."
		>
			<div className={styles.sectionContent}>
				<MessageActionBarPreview
					showActionBar={showMessageActionBar}
					showQuickReactions={showMessageActionBarQuickReactions}
					showShiftExpand={showMessageActionBarShiftExpand}
					onlyMoreButton={showMessageActionBarOnlyMoreButton}
				/>
				<SwitchGroup>
					<SwitchGroupItem
						label={t`Show message action bar`}
						value={showMessageActionBar}
						onChange={(value) => AccessibilityActionCreators.update({showMessageActionBar: value})}
					/>
					<SwitchGroupItem
						label={t`Show only More button`}
						value={showMessageActionBarOnlyMoreButton}
						onChange={(value) => AccessibilityActionCreators.update({showMessageActionBarOnlyMoreButton: value})}
						disabled={!showMessageActionBar}
					/>
					<SwitchGroupItem
						label={t`Show quick reactions`}
						value={showMessageActionBarQuickReactions}
						onChange={(value) => AccessibilityActionCreators.update({showMessageActionBarQuickReactions: value})}
						disabled={!showMessageActionBar || showMessageActionBarOnlyMoreButton}
					/>
					<SwitchGroupItem
						label={t`Enable Shift to expand`}
						value={showMessageActionBarShiftExpand}
						onChange={(value) => AccessibilityActionCreators.update({showMessageActionBarShiftExpand: value})}
						disabled={!showMessageActionBar || showMessageActionBarOnlyMoreButton}
					/>
				</SwitchGroup>
			</div>
		</SettingsTabSection>
	);
});
