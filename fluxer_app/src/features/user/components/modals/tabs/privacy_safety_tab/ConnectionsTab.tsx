// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import {SettingsTabSection} from '@app/features/app/components/dialogs/shared/SettingsTabLayout';
import Guilds from '@app/features/guild/state/Guilds';
import {
	COMMUNITY_MEMBERS_DESCRIPTOR,
	FRIENDS_OF_FRIENDS_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {Switch} from '@app/features/ui/components/form/FormSwitch';
import * as UserSettingsCommands from '@app/features/user/commands/UserSettingsCommands';
import styles from '@app/features/user/components/modals/tabs/privacy_safety_tab/ConnectionsTab.module.css';
import UserSettings from '@app/features/user/state/UserSettings';
import {FriendSourceFlags} from '@fluxer/constants/src/UserConstants';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useRef, useState} from 'react';

const ALLOW_BOTS_TO_SEND_YOU_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Allow bots to send you direct messages?',
	comment: 'Confirmation prompt in the connections tab.',
});
const ALLOW_DIRECT_MESSAGES_FROM_COMMUNITY_MEMBERS_DESCRIPTOR = msg({
	message: 'Allow direct messages from community members?',
	comment: 'Confirmation prompt in the connections tab.',
});
const BLOCK_BOTS_FROM_SENDING_YOU_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Block bots from sending you direct messages?',
	comment: 'Confirmation prompt in the connections tab. Keep the tone plain and specific.',
});
const BLOCK_DIRECT_MESSAGES_FROM_COMMUNITY_MEMBERS_DESCRIPTOR = msg({
	message: 'Block direct messages from community members?',
	comment: 'Confirmation prompt in the connections tab. Keep the tone plain and specific.',
});
const DO_YOU_ALSO_WANT_TO_ALLOW_BOTS_FROM_DESCRIPTOR = msg({
	message: 'Do you also want to allow bots from your existing communities to send you direct messages?',
	comment: 'Confirmation prompt in the connections tab.',
});
const DO_YOU_ALSO_WANT_TO_ALLOW_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Do you also want to allow direct messages from members of your existing communities?',
	comment: 'Confirmation prompt in the connections tab.',
});
const DO_YOU_ALSO_WANT_TO_BLOCK_BOTS_FROM_DESCRIPTOR = msg({
	message: 'Do you also want to block direct messages from bots in your existing communities?',
	comment: 'Confirmation prompt in the connections tab. Keep the tone plain and specific.',
});
const DO_YOU_ALSO_WANT_TO_BLOCK_DIRECT_MESSAGES_DESCRIPTOR = msg({
	message: 'Do you also want to block direct messages from members of your existing communities?',
	comment: 'Confirmation prompt in the connections tab. Keep the tone plain and specific.',
});
const ALLOW_FOR_ALL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Allow for all communities',
	comment: 'Label in the connections tab.',
});
const BLOCK_FOR_ALL_COMMUNITIES_DESCRIPTOR = msg({
	message: 'Block for all communities',
	comment: 'Button or menu action label in the connections tab. Keep it concise. Keep the tone plain and specific.',
});

interface DirectMessagesConfirmModalProps {
	allowMessages: boolean;
	onApplyToAll: () => Promise<void>;
	onSkip: () => Promise<void>;
	isBotSetting?: boolean;
}

const DirectMessagesConfirmModal = observer(
	({allowMessages, onApplyToAll, onSkip, isBotSetting = false}: DirectMessagesConfirmModalProps) => {
		const {i18n} = useLingui();
		const [submitting, setSubmitting] = useState(false);
		const initialFocusRef = useRef<HTMLButtonElement | null>(null);
		const handleApplyToAll = useCallback(async () => {
			setSubmitting(true);
			try {
				await onApplyToAll();
				ModalCommands.pop();
			} finally {
				setSubmitting(false);
			}
		}, [onApplyToAll]);
		const handleSkip = useCallback(async () => {
			setSubmitting(true);
			try {
				await onSkip();
				ModalCommands.pop();
			} finally {
				setSubmitting(false);
			}
		}, [onSkip]);
		const title = allowMessages
			? isBotSetting
				? i18n._(ALLOW_BOTS_TO_SEND_YOU_DIRECT_MESSAGES_DESCRIPTOR)
				: i18n._(ALLOW_DIRECT_MESSAGES_FROM_COMMUNITY_MEMBERS_DESCRIPTOR)
			: isBotSetting
				? i18n._(BLOCK_BOTS_FROM_SENDING_YOU_DIRECT_MESSAGES_DESCRIPTOR)
				: i18n._(BLOCK_DIRECT_MESSAGES_FROM_COMMUNITY_MEMBERS_DESCRIPTOR);
		return (
			<Modal.Root
				size="small"
				centered
				initialFocusRef={initialFocusRef}
				data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-root"
			>
				<Modal.Header
					title={title}
					data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-header"
				/>
				<Modal.Content data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-content">
					<Modal.ContentLayout data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-content-layout">
						<Modal.Description data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-description">
							{allowMessages
								? isBotSetting
									? i18n._(DO_YOU_ALSO_WANT_TO_ALLOW_BOTS_FROM_DESCRIPTOR)
									: i18n._(DO_YOU_ALSO_WANT_TO_ALLOW_DIRECT_MESSAGES_DESCRIPTOR)
								: isBotSetting
									? i18n._(DO_YOU_ALSO_WANT_TO_BLOCK_BOTS_FROM_DESCRIPTOR)
									: i18n._(DO_YOU_ALSO_WANT_TO_BLOCK_DIRECT_MESSAGES_DESCRIPTOR)}
						</Modal.Description>
						<Modal.Description
							className={styles.confirmDescription}
							data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.confirm-description"
						>
							<Trans>
								You can also change this setting per-community by right-clicking the community name and selecting
								privacy settings.
							</Trans>
						</Modal.Description>
					</Modal.ContentLayout>
				</Modal.Content>
				<Modal.Footer data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.modal-footer">
					<Button
						onClick={handleSkip}
						variant="secondary"
						disabled={submitting}
						data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.button.skip"
					>
						<Trans>Skip this step</Trans>
					</Button>
					<Button
						onClick={handleApplyToAll}
						submitting={submitting}
						ref={initialFocusRef}
						data-flx="user.privacy-safety-tab.connections-tab.direct-messages-confirm-modal.button.apply-to-all"
					>
						{allowMessages
							? i18n._(ALLOW_FOR_ALL_COMMUNITIES_DESCRIPTOR)
							: i18n._(BLOCK_FOR_ALL_COMMUNITIES_DESCRIPTOR)}
					</Button>
				</Modal.Footer>
			</Modal.Root>
		);
	},
);
export const ConnectionsTabContent: React.FC = observer(() => {
	const {i18n} = useLingui();
	const friendSourceFlags = UserSettings.getFriendSourceFlags();
	const defaultGuildsRestricted = UserSettings.getDefaultGuildsRestricted();
	const botDefaultGuildsRestricted = UserSettings.getBotDefaultGuildsRestricted();
	const everyoneEnabled = (friendSourceFlags & FriendSourceFlags.NO_RELATION) === FriendSourceFlags.NO_RELATION;
	const hasFriendFlag = (flag: number) => (friendSourceFlags & flag) === flag;
	const handleFriendRequestToggle = async (flag: number, value: boolean) => {
		let newFlags = friendSourceFlags;
		if (value) {
			newFlags |= flag;
		} else {
			newFlags &= ~flag;
		}
		await UserSettingsCommands.update({friendSourceFlags: newFlags});
	};
	const handleDirectMessagesToggle = async (value: boolean) => {
		const guilds = Guilds.getGuilds();
		const hasGuilds = guilds.length > 0;
		if (hasGuilds) {
			ModalCommands.push(
				modal(() => (
					<DirectMessagesConfirmModal
						allowMessages={value}
						onApplyToAll={async () => {
							const guildIds = value ? [] : guilds.map((guild) => guild.id);
							await UserSettingsCommands.update({defaultGuildsRestricted: !value, restrictedGuilds: guildIds});
						}}
						onSkip={async () => {
							await UserSettingsCommands.update({defaultGuildsRestricted: !value});
						}}
						data-flx="user.privacy-safety-tab.connections-tab.handle-direct-messages-toggle.direct-messages-confirm-modal"
					/>
				)),
			);
		} else {
			await UserSettingsCommands.update({defaultGuildsRestricted: !value});
		}
	};
	const handleBotDirectMessagesToggle = async (value: boolean) => {
		const guilds = Guilds.getGuilds();
		const hasGuilds = guilds.length > 0;
		if (hasGuilds) {
			ModalCommands.push(
				modal(() => (
					<DirectMessagesConfirmModal
						allowMessages={value}
						isBotSetting
						onApplyToAll={async () => {
							const guildIds = value ? [] : guilds.map((guild) => guild.id);
							await UserSettingsCommands.update({
								botDefaultGuildsRestricted: !value,
								botRestrictedGuilds: guildIds,
							});
						}}
						onSkip={async () => {
							await UserSettingsCommands.update({
								botDefaultGuildsRestricted: !value,
							});
						}}
						data-flx="user.privacy-safety-tab.connections-tab.handle-bot-direct-messages-toggle.direct-messages-confirm-modal"
					/>
				)),
			);
		} else {
			await UserSettingsCommands.update({botDefaultGuildsRestricted: !value});
		}
	};
	return (
		<>
			<SettingsTabSection
				title={<Trans>Friend requests</Trans>}
				data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.settings-tab-section"
			>
				<Switch
					label={<Trans>Everyone</Trans>}
					value={hasFriendFlag(FriendSourceFlags.NO_RELATION)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.NO_RELATION, value)}
					data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.switch.friend-request-toggle"
				/>
				<Switch
					label={i18n._(FRIENDS_OF_FRIENDS_DESCRIPTOR)}
					value={everyoneEnabled || hasFriendFlag(FriendSourceFlags.MUTUAL_FRIENDS)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.MUTUAL_FRIENDS, value)}
					disabled={everyoneEnabled}
					data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.switch.friend-request-toggle--2"
				/>
				<Switch
					label={i18n._(COMMUNITY_MEMBERS_DESCRIPTOR)}
					value={everyoneEnabled || hasFriendFlag(FriendSourceFlags.MUTUAL_GUILDS)}
					onChange={(value) => handleFriendRequestToggle(FriendSourceFlags.MUTUAL_GUILDS, value)}
					disabled={everyoneEnabled}
					data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.switch.friend-request-toggle--3"
				/>
			</SettingsTabSection>
			<SettingsTabSection
				title={<Trans>Direct messages</Trans>}
				data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.settings-tab-section--2"
			>
				<Switch
					label={<Trans>Allow direct messages from community members</Trans>}
					value={!defaultGuildsRestricted}
					onChange={handleDirectMessagesToggle}
					data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.switch.direct-messages-toggle"
				/>
				<Switch
					label={<Trans>Allow direct messages from community bots</Trans>}
					value={!botDefaultGuildsRestricted}
					onChange={handleBotDirectMessagesToggle}
					data-flx="user.privacy-safety-tab.connections-tab.connections-tab-content.switch.bot-direct-messages-toggle"
				/>
			</SettingsTabSection>
		</>
	);
});
