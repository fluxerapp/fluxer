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

import {t} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {PushPinIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as ChannelActionCreators from '~/actions/ChannelActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as PrivateChannelActionCreators from '~/actions/PrivateChannelActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {ChannelTypes, ME, RelationshipTypes} from '~/Constants';
import {DMCloseFailedModal} from '~/components/alerts/DMCloseFailedModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Routes} from '~/Routes';
import type {ChannelRecord} from '~/records/ChannelRecord';
import type {UserRecord} from '~/records/UserRecord';
import RelationshipStore from '~/stores/RelationshipStore';
import SelectedChannelStore from '~/stores/SelectedChannelStore';
import UserSettingsStore from '~/stores/UserSettingsStore';
import * as RouterUtils from '~/utils/RouterUtils';
import {StartVoiceCallMenuItem} from './items/CallMenuItems';
import {CopyChannelIdMenuItem, FavoriteChannelMenuItem} from './items/ChannelMenuItems';
import {CopyUserIdMenuItem} from './items/CopyMenuItems';
import {DebugChannelMenuItem, DebugUserMenuItem} from './items/DebugMenuItems';
import {MarkDMAsReadMenuItem, MuteDMMenuItem} from './items/DMMenuItems';
import {InviteToCommunityMenuItem} from './items/InviteMenuItems';
import {
	BlockUserMenuItem,
	ChangeFriendNicknameMenuItem,
	RelationshipActionMenuItem,
	UnblockUserMenuItem,
} from './items/RelationshipMenuItems';
import {AddNoteMenuItem} from './items/UserNoteMenuItems';
import {UserProfileMenuItem} from './items/UserProfileMenuItem';
import {MenuGroup} from './MenuGroup';
import {MenuItem} from './MenuItem';

interface DMContextMenuProps {
	channel: ChannelRecord;
	recipient?: UserRecord | null;
	onClose: () => void;
}

export const DMContextMenu: React.FC<DMContextMenuProps> = observer(({channel, recipient, onClose}) => {
	const {i18n} = useLingui();

	const isGroupDM = channel.type === ChannelTypes.GROUP_DM;
	const developerMode = UserSettingsStore.developerMode;
	const isRecipientBot = recipient?.bot;
	const relationshipType = recipient ? RelationshipStore.getRelationship(recipient.id)?.type : undefined;

	const handleCloseDM = React.useCallback(() => {
		onClose();

		const username = recipient?.username ?? '';
		const description = isGroupDM
			? t(i18n)`Are you sure you want to close this group DM? You can always reopen it later.`
			: t(i18n)`Are you sure you want to close your DM with ${username}? You can always reopen it later.`;

		ModalActionCreators.push(
			modal(() => (
				<ConfirmModal
					title={t(i18n)`Close DM`}
					description={description}
					primaryText={t(i18n)`Close DM`}
					primaryVariant="danger-primary"
					onPrimary={async () => {
						try {
							await ChannelActionCreators.remove(channel.id);

							const selectedChannel = SelectedChannelStore.selectedChannelIds.get(ME);
							if (selectedChannel === channel.id) {
								RouterUtils.transitionTo(Routes.ME);
							}

							ToastActionCreators.createToast({
								type: 'success',
								children: t(i18n)`DM closed`,
							});
						} catch (error) {
							console.error('Failed to close DM:', error);
							ModalActionCreators.push(modal(() => <DMCloseFailedModal />));
						}
					}}
				/>
			)),
		);
	}, [channel.id, i18n, isGroupDM, onClose, recipient?.username]);

	const handlePinDM = React.useCallback(async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.pinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: t(i18n)`Pinned DM`,
			});
		} catch (error) {
			console.error('Failed to pin DM:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t(i18n)`Failed to pin DM`,
			});
		}
	}, [channel.id, i18n, onClose]);

	const handleUnpinDM = React.useCallback(async () => {
		onClose();
		try {
			await PrivateChannelActionCreators.unpinDmChannel(channel.id);
			ToastActionCreators.createToast({
				type: 'success',
				children: t(i18n)`Unpinned DM`,
			});
		} catch (error) {
			console.error('Failed to unpin DM:', error);
			ToastActionCreators.createToast({
				type: 'error',
				children: t(i18n)`Failed to unpin DM`,
			});
		}
	}, [channel.id, i18n, onClose]);

	return (
		<>
			<MenuGroup>
				<MarkDMAsReadMenuItem channel={channel} onClose={onClose} />
				{channel.isPinned ? (
					<MenuItem icon={<PushPinIcon />} onClick={handleUnpinDM}>
						{t(i18n)`Unpin DM`}
					</MenuItem>
				) : (
					<MenuItem icon={<PushPinIcon />} onClick={handlePinDM}>
						{t(i18n)`Pin DM`}
					</MenuItem>
				)}
			</MenuGroup>

			<MenuGroup>
				<FavoriteChannelMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			{recipient && (
				<MenuGroup>
					<UserProfileMenuItem user={recipient} onClose={onClose} />
					{!isRecipientBot && <StartVoiceCallMenuItem user={recipient} onClose={onClose} />}
					<AddNoteMenuItem user={recipient} onClose={onClose} />
					<ChangeFriendNicknameMenuItem user={recipient} onClose={onClose} />
					<MenuItem icon={<XIcon weight="bold" />} onClick={handleCloseDM}>
						{t(i18n)`Close DM`}
					</MenuItem>
				</MenuGroup>
			)}

			{recipient && (
				<MenuGroup>
					{!isRecipientBot && <InviteToCommunityMenuItem user={recipient} onClose={onClose} />}
					{!isRecipientBot && <RelationshipActionMenuItem user={recipient} onClose={onClose} />}
					{relationshipType === RelationshipTypes.BLOCKED ? (
						<UnblockUserMenuItem user={recipient} onClose={onClose} />
					) : (
						<BlockUserMenuItem user={recipient} onClose={onClose} />
					)}
				</MenuGroup>
			)}

			<MenuGroup>
				<MuteDMMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>

			{developerMode && (
				<MenuGroup>
					{recipient && <DebugUserMenuItem user={recipient} onClose={onClose} />}
					<DebugChannelMenuItem channel={channel} onClose={onClose} />
				</MenuGroup>
			)}

			<MenuGroup>
				{recipient && <CopyUserIdMenuItem user={recipient} onClose={onClose} />}
				<CopyChannelIdMenuItem channel={channel} onClose={onClose} />
			</MenuGroup>
		</>
	);
});
