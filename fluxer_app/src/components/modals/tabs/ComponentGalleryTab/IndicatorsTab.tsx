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
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {ChannelTypes, getStatusTypeLabel, StatusTypes} from '~/Constants';
import {GroupDMAvatar} from '~/components/common/GroupDMAvatar';
import {
	SettingsTabContainer,
	SettingsTabContent,
	SettingsTabSection,
} from '~/components/modals/shared/SettingsTabLayout';
import {AvatarStack} from '~/components/uikit/avatars/AvatarStack';
import {MentionBadge} from '~/components/uikit/MentionBadge';
import {MockAvatar} from '~/components/uikit/MockAvatar';
import {ChannelRecord} from '~/records/ChannelRecord';
import type {UserPartial} from '~/records/UserRecord';
import styles from './IndicatorsTab.module.css';
import {SubsectionTitle} from './shared';

const AVATAR_SIZES_WITH_STATUS: Array<16 | 24 | 32 | 36 | 40 | 48 | 80> = [16, 24, 32, 36, 40, 48, 80];

const AVATAR_STATUSES: Array<string> = [
	StatusTypes.ONLINE,
	StatusTypes.IDLE,
	StatusTypes.DND,
	StatusTypes.INVISIBLE,
	StatusTypes.OFFLINE,
];

const createMockRecipient = (id: string): UserPartial => ({
	id,
	username: id,
	discriminator: '0000',
	avatar: null,
	flags: 0,
});

const createMockGroupDMChannel = (id: string, recipientIds: Array<string>): ChannelRecord =>
	new ChannelRecord({
		id,
		type: ChannelTypes.GROUP_DM,
		recipients: recipientIds.map(createMockRecipient),
	});

const getMockGroupDMChannels = (): Array<ChannelRecord> => [
	createMockGroupDMChannel('1000000000000000001', ['1000000000000000002', '1000000000000000003']),
	createMockGroupDMChannel('1000000000000000004', [
		'1000000000000000005',
		'1000000000000000006',
		'1000000000000000007',
	]),
];

export const IndicatorsTab: React.FC = observer(() => {
	const {i18n} = useLingui();

	const mockGroupDMChannels = getMockGroupDMChannels();

	return (
		<SettingsTabContainer>
			<SettingsTabContent>
				<SettingsTabSection
					title={<Trans>Status Indicators</Trans>}
					description={
						<Trans>
							Visual indicators showing user status, rendered using the same masked status badges as avatars: online,
							idle, do not disturb, invisible, and offline.
						</Trans>
					}
				>
					<SubsectionTitle>
						<Trans>Single User (All Statuses)</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_STATUSES.map((status) => (
							<div key={status} className={styles.avatarGroup}>
								<MockAvatar size={40} status={status} />
								<span className={styles.itemTextTertiary}>{getStatusTypeLabel(i18n, status) ?? status}</span>
							</div>
						))}
					</div>

					<SubsectionTitle>
						<Trans>Mobile Online Status on Avatars</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_SIZES_WITH_STATUS.map((size) => (
							<div key={`mobile-avatar-size-${size}`} className={styles.avatarGroup}>
								<MockAvatar size={size} status={StatusTypes.ONLINE} isMobileStatus />
								<span className={styles.itemTextTertiary}>{size}px</span>
							</div>
						))}
					</div>

					<SubsectionTitle>
						<Trans>Different Sizes (Status Supported)</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_SIZES_WITH_STATUS.map((size) => (
							<div key={size} className={styles.avatarGroup}>
								<MockAvatar size={size} status={StatusTypes.ONLINE} />
								<span className={styles.itemTextTertiary}>{size}px</span>
							</div>
						))}
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Mention Badges</Trans>}
					description={<Trans>Notification badges showing unread mention counts in different sizes.</Trans>}
				>
					<SubsectionTitle>
						<Trans>Medium Size (Default)</Trans>
					</SubsectionTitle>
					<div className={styles.badgesWrapper}>
						<MentionBadge mentionCount={1} />
						<MentionBadge mentionCount={5} />
						<MentionBadge mentionCount={12} />
						<MentionBadge mentionCount={99} />
						<MentionBadge mentionCount={150} />
						<MentionBadge mentionCount={1000} />
						<MentionBadge mentionCount={9999} />
					</div>

					<SubsectionTitle>
						<Trans>Small Size</Trans>
					</SubsectionTitle>
					<div className={styles.badgesWrapper}>
						<MentionBadge mentionCount={1} size="small" />
						<MentionBadge mentionCount={5} size="small" />
						<MentionBadge mentionCount={12} size="small" />
						<MentionBadge mentionCount={99} size="small" />
						<MentionBadge mentionCount={150} size="small" />
						<MentionBadge mentionCount={1000} size="small" />
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Mock Avatars</Trans>}
					description={<Trans>Mock user avatars in various sizes and all status permutations.</Trans>}
				>
					<SubsectionTitle>
						<Trans>Different Sizes (Online)</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_SIZES_WITH_STATUS.map((size) => (
							<div key={size} className={styles.avatarGroup}>
								<MockAvatar size={size} status={StatusTypes.ONLINE} />
								<span className={styles.itemTextTertiary}>{size}px</span>
							</div>
						))}
					</div>

					<SubsectionTitle>
						<Trans>All Status Types</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_STATUSES.map((status) => (
							<div key={status} className={styles.avatarGroup}>
								<MockAvatar size={48} status={status} />
								<span className={styles.itemTextTertiary}>{getStatusTypeLabel(i18n, status) ?? status}</span>
							</div>
						))}
					</div>

					<SubsectionTitle>
						<Trans>Typing State</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						{AVATAR_STATUSES.map((status) => (
							<div key={status} className={styles.avatarGroup}>
								<MockAvatar size={48} status={status} isTyping />
								<span className={styles.itemTextTertiary}>{getStatusTypeLabel(i18n, status) ?? status}</span>
							</div>
						))}
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Group DM Avatars</Trans>}
					description={
						<Trans>
							Group DM avatars using the same status masks as regular avatars, including stacked layouts and typing
							states.
						</Trans>
					}
				>
					<SubsectionTitle>
						<Trans>Different Sizes & Member Counts</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[0]} size={32} />
							<span className={styles.itemTextTertiary}>32px · 2 members</span>
						</div>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[1]} size={40} />
							<span className={styles.itemTextTertiary}>40px · 3 members</span>
						</div>
					</div>

					<SubsectionTitle>
						<Trans>Group Online Status</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[0]} size={32} statusOverride={StatusTypes.ONLINE} />
							<span className={styles.itemTextTertiary}>
								<Trans>2 members (online)</Trans>
							</span>
						</div>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[1]} size={40} statusOverride={StatusTypes.ONLINE} />
							<span className={styles.itemTextTertiary}>
								<Trans>3 members (online)</Trans>
							</span>
						</div>
					</div>

					<SubsectionTitle>
						<Trans>Group Typing States</Trans>
					</SubsectionTitle>
					<div className={styles.itemsWrapper}>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[0]} size={32} isTyping />
							<span className={styles.itemTextTertiary}>
								<Trans>2 members (typing)</Trans>
							</span>
						</div>
						<div className={styles.avatarGroup}>
							<GroupDMAvatar channel={mockGroupDMChannels[1]} size={40} isTyping />
							<span className={styles.itemTextTertiary}>
								<Trans>3 members (typing)</Trans>
							</span>
						</div>
					</div>
				</SettingsTabSection>

				<SettingsTabSection
					title={<Trans>Avatar Stacks</Trans>}
					description={<Trans>Overlapping avatar groups showing multiple users with automatic overflow counts.</Trans>}
				>
					<SubsectionTitle>
						<Trans>Different Sizes</Trans>
					</SubsectionTitle>
					<div className={styles.stacksWrapper}>
						<div className={styles.stackItem}>
							<AvatarStack size={24}>
								{[1, 2, 3, 4].map((i) => (
									<MockAvatar key={i} size={24} userTag={`User ${i}`} />
								))}
							</AvatarStack>
							<span className={styles.itemTextTertiary}>24px</span>
						</div>
						<div className={styles.stackItem}>
							<AvatarStack size={32}>
								{[1, 2, 3, 4].map((i) => (
									<MockAvatar key={i} size={32} userTag={`User ${i}`} />
								))}
							</AvatarStack>
							<span className={styles.itemTextTertiary}>32px</span>
						</div>
						<div className={styles.stackItem}>
							<AvatarStack size={40}>
								{[1, 2, 3, 4].map((i) => (
									<MockAvatar key={i} size={40} userTag={`User ${i}`} />
								))}
							</AvatarStack>
							<span className={styles.itemTextTertiary}>40px</span>
						</div>
					</div>

					<SubsectionTitle>
						<Trans>Max Visible Count</Trans>
					</SubsectionTitle>
					<div className={styles.stacksWrapper}>
						<div className={styles.stackItem}>
							<AvatarStack size={32} maxVisible={3}>
								{[1, 2, 3, 4, 5, 6, 7, 8].map((i) => (
									<MockAvatar key={i} size={32} userTag={`User ${i}`} />
								))}
							</AvatarStack>
							<span className={styles.itemTextTertiary}>
								<Trans>Show max 3 (+5 badge)</Trans>
							</span>
						</div>
						<div className={styles.stackItem}>
							<AvatarStack size={32} maxVisible={5}>
								{[1, 2, 3, 4, 5, 6, 7, 8, 9, 10].map((i) => (
									<MockAvatar key={i} size={32} userTag={`User ${i}`} />
								))}
							</AvatarStack>
							<span className={styles.itemTextTertiary}>
								<Trans>Show max 5 (+5 badge)</Trans>
							</span>
						</div>
					</div>
				</SettingsTabSection>
			</SettingsTabContent>
		</SettingsTabContainer>
	);
});
