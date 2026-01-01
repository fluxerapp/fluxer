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
import React from 'react';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as UserSettingsActionCreators from '~/actions/UserSettingsActionCreators';
import {FriendSourceFlags} from '~/Constants';
import {Switch} from '~/components/form/Switch';
import confirmStyles from '~/components/modals/ConfirmModal.module.css';
import * as Modal from '~/components/modals/Modal';
import {SettingsTabSection} from '~/components/modals/shared/SettingsTabLayout';
import {Button} from '~/components/uikit/Button/Button';
import GuildStore from '~/stores/GuildStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import styles from './ConnectionsTab.module.css';

const DirectMessagesConfirmModal = observer(
	({
		allowMessages,
		onApplyToAll,
		onSkip,
	}: {
		allowMessages: boolean;
		onApplyToAll: () => Promise<void>;
		onSkip: () => Promise<void>;
	}) => {
		const {t} = useLingui();
		const [submitting, setSubmitting] = React.useState(false);
		const initialFocusRef = React.useRef<HTMLButtonElement | null>(null);

		const handleApplyToAll = React.useCallback(async () => {
			setSubmitting(true);
			try {
				await onApplyToAll();
				ModalActionCreators.pop();
			} finally {
				setSubmitting(false);
			}
		}, [onApplyToAll]);

		const handleSkip = React.useCallback(async () => {
			setSubmitting(true);
			try {
				await onSkip();
				ModalActionCreators.pop();
			} finally {
				setSubmitting(false);
			}
		}, [onSkip]);

		const title = allowMessages
			? t`Allow direct messages from community members?`
			: t`Block direct messages from community members?`;

		return (
			<Modal.Root size="small" centered initialFocusRef={initialFocusRef}>
				<Modal.Header title={title} />
				<Modal.Content className={confirmStyles.content}>
					<p className={confirmStyles.descriptionText}>
						{allowMessages
							? t`Do you also want to allow direct messages from members of your existing communities?`
							: t`Do you also want to block direct messages from members of your existing communities?`}
					</p>
					<p className={clsx(styles.confirmDescription, confirmStyles.descriptionText)}>
						<Trans>
							You can also change this setting per-community by right-clicking the community name and selecting Privacy
							Settings.
						</Trans>
					</p>
				</Modal.Content>
				<Modal.Footer>
					<Button onClick={handleSkip} variant="secondary" disabled={submitting}>
						<Trans>Skip this step</Trans>
					</Button>
					<Button onClick={handleApplyToAll} submitting={submitting} ref={initialFocusRef}>
						{allowMessages ? t`Allow for all communities` : t`Block for all communities`}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);

export const ConnectionsTabContent: React.FC = observer(() => {
	const friendSourceFlags = UserSettingsStore.getFriendSourceFlags();
	const defaultGuildsRestricted = UserSettingsStore.getDefaultGuildsRestricted();

	const hasFriendFlag = (flag: number) => (friendSourceFlags & flag) === flag;

	const handleFriendRequestToggle = async (flag: number, value: boolean) => {
		let newFlags = friendSourceFlags;

		if (value) {
			newFlags |= flag;

			if (flag === FriendSourceFlags.NO_RELATION) {
				newFlags |= FriendSourceFlags.MUTUAL_FRIENDS;
				newFlags |= FriendSourceFlags.MUTUAL_GUILDS;
			}
		} else {
			newFlags &= ~flag;

			if (flag === FriendSourceFlags.MUTUAL_FRIENDS || flag === FriendSourceFlags.MUTUAL_GUILDS) {
				newFlags &= ~FriendSourceFlags.NO_RELATION;
			}
		}

		await UserSettingsActionCreators.update({friendSourceFlags: newFlags});
	};

	const handleDirectMessagesToggle = async (value: boolean) => {
		const guilds = GuildStore.getGuilds();
		const hasGuilds = guilds.length > 0;

		if (hasGuilds) {
			ModalActionCreators.push(
				modal(() => (
					<DirectMessagesConfirmModal
						allowMessages={value}
						onApplyToAll={async () => {
							const guildIds = value ? [] : guilds.map((guild) => guild.id);
							await UserSettingsActionCreators.update({defaultGuildsRestricted: !value, restrictedGuilds: guildIds});
						}}
						onSkip={async () => {
							await UserSettingsActionCreators.update({defaultGuildsRestricted: !value});
						}}
					/>
				)),
			);
		} else {
			await UserSettingsActionCreators.update({defaultGuildsRestricted: !value});
		}
	};

	return (
		<>
			<SettingsTabSection
				title={<Trans>Friend Requests</Trans>}
				description={<Trans>Control who can send you friend requests</Trans>}
			>
				<Switch
					label={<Trans>Everyone</Trans>}
					description={<Trans>Allow anyone to send you friend requests</Trans>}
					value={hasFriendFlag(FriendSourceFlags.NO_RELATION)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.NO_RELATION, value)}
				/>
				<Switch
					label={<Trans>Friends of Friends</Trans>}
					description={<Trans>Allow friends of your friends to send you requests</Trans>}
					value={hasFriendFlag(FriendSourceFlags.MUTUAL_FRIENDS)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.MUTUAL_FRIENDS, value)}
				/>
				<Switch
					label={<Trans>Community Members</Trans>}
					description={<Trans>Allow members from communities you're in to send you requests</Trans>}
					value={hasFriendFlag(FriendSourceFlags.MUTUAL_GUILDS)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.MUTUAL_GUILDS, value)}
				/>
			</SettingsTabSection>

			<SettingsTabSection
				title={<Trans>Direct Messages</Trans>}
				description={<Trans>Control who can send you direct messages</Trans>}
			>
				<Switch
					label={<Trans>Allow direct messages from community members</Trans>}
					description={<Trans>Allow members from communities you're in to send you direct messages</Trans>}
					value={!defaultGuildsRestricted}
					onChange={handleDirectMessagesToggle}
				/>
			</SettingsTabSection>
		</>
	);
});
