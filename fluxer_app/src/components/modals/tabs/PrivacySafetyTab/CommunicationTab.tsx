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
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {GroupDmAddPermissionFlags, IncomingCallFlags} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {RadioGroup} from '~/components/uikit/RadioGroup/RadioGroup';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './CommunicationTab.module.css';

export const CommunicationTabContent: React.FC = observer(() => {
	const {t} = useLingui();
	const incomingCallFlags = UserSettingsStore.getIncomingCallFlags();
	const groupDmAddPermissionFlags = UserSettingsStore.getGroupDmAddPermissionFlags();

	const hasCallFlag = (flag: number) => (incomingCallFlags & flag) === flag;
	const hasGroupDmAddFlag = (flag: number) => (groupDmAddPermissionFlags & flag) === flag;

	const getIncomingCallBaseValue = (): 'nobody' | 'friends_only' | 'everyone' | 'custom' => {
		if (hasCallFlag(IncomingCallFlags.NOBODY)) return 'nobody';
		if (hasCallFlag(IncomingCallFlags.EVERYONE)) return 'everyone';
		if (hasCallFlag(IncomingCallFlags.FRIENDS_ONLY)) return 'friends_only';
		return 'custom';
	};

	const handleIncomingCallBaseChange = async (value: 'nobody' | 'friends_only' | 'everyone' | 'custom') => {
		let newFlags = 0;
		if (value === 'nobody') {
			newFlags = IncomingCallFlags.NOBODY;
		} else if (value === 'friends_only') {
			newFlags = IncomingCallFlags.FRIENDS_ONLY;
		} else if (value === 'everyone') {
			newFlags = IncomingCallFlags.EVERYONE;
		} else {
			newFlags = incomingCallFlags & (IncomingCallFlags.FRIENDS_OF_FRIENDS | IncomingCallFlags.GUILD_MEMBERS);
			if (newFlags === 0) {
				newFlags = IncomingCallFlags.FRIENDS_OF_FRIENDS;
			}
		}
		await UserSettingsActionCreators.update({incomingCallFlags: newFlags});
	};

	const handleIncomingCallAdditiveToggle = async (flag: number, value: boolean) => {
		let newFlags = incomingCallFlags;

		newFlags &= ~IncomingCallFlags.NOBODY;
		newFlags &= ~IncomingCallFlags.FRIENDS_ONLY;
		newFlags &= ~IncomingCallFlags.EVERYONE;

		if (value) {
			newFlags |= flag;
		} else {
			newFlags &= ~flag;
		}

		if (newFlags === 0) {
			newFlags = IncomingCallFlags.FRIENDS_ONLY;
		}

		await UserSettingsActionCreators.update({incomingCallFlags: newFlags});
	};

	const handleIncomingCallModifierToggle = async (flag: number, value: boolean) => {
		let newFlags = incomingCallFlags;

		if (value) {
			newFlags |= flag;
		} else {
			newFlags &= ~flag;
		}

		await UserSettingsActionCreators.update({incomingCallFlags: newFlags});
	};

	const getGroupDmAddBaseValue = (): 'nobody' | 'friends_only' | 'everyone' | 'custom' => {
		if (hasGroupDmAddFlag(GroupDmAddPermissionFlags.NOBODY)) return 'nobody';
		if (hasGroupDmAddFlag(GroupDmAddPermissionFlags.EVERYONE)) return 'everyone';
		if (hasGroupDmAddFlag(GroupDmAddPermissionFlags.FRIENDS_ONLY)) return 'friends_only';
		return 'custom';
	};

	const handleGroupDmAddBaseChange = async (value: 'nobody' | 'friends_only' | 'everyone' | 'custom') => {
		let newFlags = 0;
		if (value === 'nobody') {
			newFlags = GroupDmAddPermissionFlags.NOBODY;
		} else if (value === 'friends_only') {
			newFlags = GroupDmAddPermissionFlags.FRIENDS_ONLY;
		} else if (value === 'everyone') {
			newFlags = GroupDmAddPermissionFlags.EVERYONE;
		} else {
			newFlags =
				groupDmAddPermissionFlags &
				(GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS | GroupDmAddPermissionFlags.GUILD_MEMBERS);
			if (newFlags === 0) {
				newFlags = GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS;
			}
		}
		await UserSettingsActionCreators.update({groupDmAddPermissionFlags: newFlags});
	};

	const handleGroupDmAddAdditiveToggle = async (flag: number, value: boolean) => {
		let newFlags = groupDmAddPermissionFlags;

		newFlags &= ~GroupDmAddPermissionFlags.NOBODY;
		newFlags &= ~GroupDmAddPermissionFlags.FRIENDS_ONLY;
		newFlags &= ~GroupDmAddPermissionFlags.EVERYONE;

		if (value) {
			newFlags |= flag;
		} else {
			newFlags &= ~flag;
		}

		if (newFlags === 0) {
			newFlags = GroupDmAddPermissionFlags.FRIENDS_ONLY;
		}

		await UserSettingsActionCreators.update({groupDmAddPermissionFlags: newFlags});
	};

	return (
		<>
			<SettingsTabSection title={<Trans>Incoming Calls</Trans>} description={<Trans>Control who can call you</Trans>}>
				<RadioGroup
					value={getIncomingCallBaseValue()}
					onChange={handleIncomingCallBaseChange}
					aria-label={t`Incoming call permissions`}
					options={[
						{
							value: 'nobody',
							name: t`Nobody`,
							desc: t`Block all incoming calls`,
						},
						{
							value: 'friends_only',
							name: t`Friends Only`,
							desc: t`Only allow friends to call you (recommended)`,
						},
						{
							value: 'custom',
							name: t`Friends + Custom`,
							desc: t`Allow friends plus additional groups you choose`,
						},
						{
							value: 'everyone',
							name: t`Everyone`,
							desc: t`Allow anyone to call you, even strangers`,
						},
					]}
				/>

				{getIncomingCallBaseValue() === 'custom' && (
					<>
						<div className={styles.subsectionHeader}>
							<Trans>Additional Groups</Trans>
						</div>
						<Switch
							label={<Trans>Friends of Friends</Trans>}
							description={<Trans>People who are friends with your friends can call you</Trans>}
							value={hasCallFlag(IncomingCallFlags.FRIENDS_OF_FRIENDS)}
							onChange={(value) => handleIncomingCallAdditiveToggle(IncomingCallFlags.FRIENDS_OF_FRIENDS, value)}
						/>
						<Switch
							label={<Trans>Community Members</Trans>}
							description={<Trans>People from communities you're both in can call you</Trans>}
							value={hasCallFlag(IncomingCallFlags.GUILD_MEMBERS)}
							onChange={(value) => handleIncomingCallAdditiveToggle(IncomingCallFlags.GUILD_MEMBERS, value)}
						/>
					</>
				)}

				{getIncomingCallBaseValue() !== 'nobody' && (
					<>
						<div className={styles.subsectionHeader}>
							<Trans>Ring Behavior</Trans>
						</div>
						<Switch
							label={<Trans>Silent calls from everyone</Trans>}
							description={
								<Trans>
									All calls will notify silently instead of ringing. By default, calls from non-friends are always
									silent.
								</Trans>
							}
							value={hasCallFlag(IncomingCallFlags.SILENT_EVERYONE)}
							onChange={(value) => handleIncomingCallModifierToggle(IncomingCallFlags.SILENT_EVERYONE, value)}
						/>
					</>
				)}
			</SettingsTabSection>

			<SettingsTabSection
				title={<Trans>Who Can Add You to Group Chats</Trans>}
				description={
					<Trans>
						Control who can add you to group chats without asking. Anyone can still send you invite links to join.
					</Trans>
				}
			>
				<RadioGroup
					value={getGroupDmAddBaseValue()}
					onChange={handleGroupDmAddBaseChange}
					aria-label={t`Group chat add permissions`}
					options={[
						{
							value: 'nobody',
							name: t`Nobody`,
							desc: t`Don't let anyone add you to group chats without asking`,
						},
						{
							value: 'friends_only',
							name: t`Friends Only`,
							desc: t`Only allow friends to add you without asking (recommended)`,
						},
						{
							value: 'custom',
							name: t`Friends + Custom`,
							desc: t`Allow friends plus additional groups to add you`,
						},
						{
							value: 'everyone',
							name: t`Everyone`,
							desc: t`Allow anyone to add you to group chats without asking`,
						},
					]}
				/>

				{getGroupDmAddBaseValue() === 'custom' && (
					<>
						<div className={styles.subsectionHeader}>
							<Trans>Additional Groups</Trans>
						</div>
						<Switch
							label={<Trans>Friends of Friends</Trans>}
							description={<Trans>People who are friends with your friends can add you to group chats</Trans>}
							value={hasGroupDmAddFlag(GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS)}
							onChange={(value) => handleGroupDmAddAdditiveToggle(GroupDmAddPermissionFlags.FRIENDS_OF_FRIENDS, value)}
						/>
						<Switch
							label={<Trans>Community Members</Trans>}
							description={<Trans>People from communities you're both in can add you to group chats</Trans>}
							value={hasGroupDmAddFlag(GroupDmAddPermissionFlags.GUILD_MEMBERS)}
							onChange={(value) => handleGroupDmAddAdditiveToggle(GroupDmAddPermissionFlags.GUILD_MEMBERS, value)}
						/>
					</>
				)}
			</SettingsTabSection>
		</>
	);
});
