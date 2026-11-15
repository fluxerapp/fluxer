// SPDX-License-Identifier: AGPL-3.0-or-later

import modalI18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import {StatusSlate} from '@app/features/app/components/dialogs/shared/StatusSlate';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import * as ChannelCommands from '@app/features/channel/commands/ChannelCommands';
import styles from '@app/features/channel/components/modals/channel_tabs/ChannelInvitesTab.module.css';
import Channels from '@app/features/channel/state/Channels';
import {
	TRY_AGAIN_DESCRIPTOR,
	TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR,
} from '@app/features/i18n/utils/CommonMessageDescriptors';
import {DisableInvitesButton} from '@app/features/invite/components/DisableInvitesButton';
import {InviteDateToggle} from '@app/features/invite/components/InviteDateToggle';
import {InviteListHeader, InviteListItem} from '@app/features/invite/components/InviteListItem';
import {InviteModal} from '@app/features/invite/components/modals/InviteModal';
import {useInviteRevoke} from '@app/features/invite/hooks/useInviteRevoke';
import Invites from '@app/features/invite/state/Invites';
import * as InviteUtils from '@app/features/invite/utils/InviteUtils';
import Permission from '@app/features/permissions/state/Permission';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {CopyLinkIcon, CopyTextIcon, DeleteIcon} from '@app/features/ui/action_menu/ContextMenuIcons';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Spinner} from '@app/features/ui/components/Spinner';
import type {MenuGroupType} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {MenuBottomSheet} from '@app/features/ui/menu_bottom_sheet/MenuBottomSheet';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {UserPlusIcon, WarningOctagonIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useMemo, useState} from 'react';

const InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR = msg({
	message: "Couldn't load invites",
	comment: 'Error modal title shown when fetching invites fails.',
});

const DELETE_INVITE_DESCRIPTOR = msg({
	message: 'Delete invite',
	comment: 'Button or menu action label in the channel invites tab. Keep it concise. Keep the tone plain and specific.',
});
const ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR = msg({
	message: "Delete this invite? Can't be undone.",
	comment: 'Error message in the channel invites tab. Keep the tone plain and specific.',
});
const COPY_INVITE_CODE_DESCRIPTOR = msg({
	message: 'Copy invite code',
	comment: 'Button or menu action label in the channel invites tab. Keep it concise.',
});
const COPY_INVITE_URL_DESCRIPTOR = msg({
	message: 'Copy invite URL',
	comment: 'Button or menu action label in the channel invites tab. Keep it concise.',
});
const ChannelInvitesTab: React.FC<{channelId: string}> = observer(({channelId}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const invites = Invites.channelInvites.get(channelId) ?? null;
	const fetchStatus = Invites.getChannelInvitesFetchStatus(channelId);
	const handleRevoke = useInviteRevoke();
	const [showCreatedDate, setShowCreatedDate] = useState(false);
	const [selectedInvite, setSelectedInvite] = useState<Invite | null>(null);
	const hideInviteLinks = StreamerMode.shouldHideInviteLinks;
	const canInvite = InviteUtils.canInviteToChannel(channelId, channel?.guildId);
	const canManageGuild = Permission.can(Permissions.MANAGE_GUILD, {
		guildId: channel?.guildId,
	});
	const refreshInvites = useCallback(async () => {
		try {
			await ChannelCommands.fetchChannelInvites(channelId);
		} catch (_error) {
			showGenericErrorModal({
				title: () => modalI18n._(InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR),
				message: () => modalI18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'invite.invites-load-failed-modal.generic-error-modal',
			});
		}
	}, [channelId]);
	useEffect(() => {
		if (fetchStatus === 'idle') {
			void refreshInvites();
		}
	}, [channelId, fetchStatus, refreshInvites]);
	const handleCreateInvite = useCallback(() => {
		ModalCommands.push(
			modal(() => (
				<InviteModal
					channelId={channelId}
					data-flx="channel.channel-tabs.channel-invites-tab.handle-create-invite.invite-modal"
				/>
			)),
		);
	}, [channelId]);
	const handleCloseInviteActions = useCallback(() => {
		setSelectedInvite(null);
	}, []);
	const handleInvitePress = useCallback((invite: Invite) => {
		setSelectedInvite(invite);
	}, []);
	const handleCopyInviteCode = useCallback(() => {
		if (hideInviteLinks) {
			handleCloseInviteActions();
			return;
		}
		if (!selectedInvite) {
			return;
		}
		void TextCopyCommands.copy(i18n, selectedInvite.code);
		handleCloseInviteActions();
	}, [handleCloseInviteActions, hideInviteLinks, i18n, selectedInvite]);
	const handleCopyInviteUrl = useCallback(() => {
		if (hideInviteLinks) {
			handleCloseInviteActions();
			return;
		}
		if (!selectedInvite) {
			return;
		}
		void TextCopyCommands.copy(i18n, `${RuntimeConfig.inviteEndpoint}/${selectedInvite.code}`);
		handleCloseInviteActions();
	}, [handleCloseInviteActions, hideInviteLinks, i18n, selectedInvite]);
	const handleDeleteInvite = useCallback(() => {
		if (!selectedInvite) {
			return;
		}
		const inviteCode = selectedInvite.code;
		handleCloseInviteActions();
		ModalCommands.push(
			modal(() => (
				<ConfirmModal
					title={i18n._(DELETE_INVITE_DESCRIPTOR)}
					description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_DELETE_THIS_DESCRIPTOR)}
					primaryText={i18n._(DELETE_INVITE_DESCRIPTOR)}
					onPrimary={() => handleRevoke(inviteCode)}
					data-flx="channel.channel-tabs.channel-invites-tab.handle-delete-invite.confirm-modal"
				/>
			)),
		);
	}, [handleCloseInviteActions, handleRevoke, selectedInvite, i18n]);
	const inviteActionGroups = useMemo<Array<MenuGroupType>>(() => {
		if (!selectedInvite) {
			return [];
		}
		const copyItems = hideInviteLinks
			? []
			: [
					{
						icon: (
							<CopyTextIcon
								size={20}
								data-flx="channel.channel-tabs.channel-invites-tab.invite-action-groups.copy-text-icon"
							/>
						),
						label: i18n._(COPY_INVITE_CODE_DESCRIPTOR),
						onClick: handleCopyInviteCode,
					},
					{
						icon: (
							<CopyLinkIcon
								size={20}
								data-flx="channel.channel-tabs.channel-invites-tab.invite-action-groups.copy-link-icon"
							/>
						),
						label: i18n._(COPY_INVITE_URL_DESCRIPTOR),
						onClick: handleCopyInviteUrl,
					},
				];
		const groups: Array<MenuGroupType> = [];
		if (copyItems.length > 0) {
			groups.push({items: copyItems});
		}
		groups.push({
			items: [
				{
					icon: (
						<DeleteIcon
							size={20}
							data-flx="channel.channel-tabs.channel-invites-tab.invite-action-groups.delete-icon"
						/>
					),
					label: i18n._(DELETE_INVITE_DESCRIPTOR),
					onClick: handleDeleteInvite,
					danger: true,
				},
			],
		});
		return groups;
	}, [handleCopyInviteCode, handleCopyInviteUrl, handleDeleteInvite, hideInviteLinks, selectedInvite, i18n.locale]);
	return (
		<div className={styles.container} data-flx="channel.channel-tabs.channel-invites-tab.container">
			<div data-flx="channel.channel-tabs.channel-invites-tab.div">
				<h2 className={styles.header} data-flx="channel.channel-tabs.channel-invites-tab.header">
					<Trans>Invites</Trans>
				</h2>
				<p className={styles.description} data-flx="channel.channel-tabs.channel-invites-tab.description">
					<Trans>Manage invite links for this channel.</Trans>
				</p>
			</div>
			<div className={styles.buttonGroup} data-flx="channel.channel-tabs.channel-invites-tab.button-group">
				<Button
					small={true}
					disabled={!canInvite || fetchStatus === 'pending'}
					onClick={handleCreateInvite}
					data-flx="channel.channel-tabs.channel-invites-tab.button.create-invite"
				>
					<Trans>Create invite</Trans>
				</Button>
				{canManageGuild && channel?.guildId && (
					<DisableInvitesButton
						guildId={channel.guildId}
						data-flx="channel.channel-tabs.channel-invites-tab.disable-invites-button"
					/>
				)}
			</div>
			{fetchStatus === 'pending' && (
				<div className={styles.spinnerContainer} data-flx="channel.channel-tabs.channel-invites-tab.spinner-container">
					<Spinner data-flx="channel.channel-tabs.channel-invites-tab.spinner" />
				</div>
			)}
			{fetchStatus === 'success' && invites && invites.length > 0 && (
				<div className={styles.invitesContainer} data-flx="channel.channel-tabs.channel-invites-tab.invites-container">
					<InviteDateToggle
						showCreatedDate={showCreatedDate}
						onToggle={setShowCreatedDate}
						data-flx="channel.channel-tabs.channel-invites-tab.invite-date-toggle"
					/>
					<div className={styles.invitesList} data-flx="channel.channel-tabs.channel-invites-tab.invites-list">
						<InviteListHeader
							showCreatedDate={showCreatedDate}
							data-flx="channel.channel-tabs.channel-invites-tab.invite-list-header"
						/>
						<div className={styles.inviteItems} data-flx="channel.channel-tabs.channel-invites-tab.invite-items">
							{invites.map((invite) => (
								<InviteListItem
									key={invite.code}
									invite={invite}
									onRevoke={handleRevoke}
									onMobilePress={handleInvitePress}
									showCreatedDate={showCreatedDate}
									data-flx="channel.channel-tabs.channel-invites-tab.invite-list-item"
								/>
							))}
						</div>
					</div>
				</div>
			)}
			{fetchStatus === 'success' && invites && invites.length === 0 && (
				<StatusSlate
					Icon={UserPlusIcon}
					title={<Trans>No invite links</Trans>}
					description={
						<Trans>This channel doesn't have any invite links yet. Create one to invite people to this channel.</Trans>
					}
					actions={
						canInvite
							? [
									{
										text: <Trans>Create invite</Trans>,
										onClick: handleCreateInvite,
										variant: 'primary',
									},
								]
							: undefined
					}
					fullHeight={true}
					data-flx="channel.channel-tabs.channel-invites-tab.status-slate"
				/>
			)}
			{fetchStatus === 'error' && (
				<StatusSlate
					Icon={WarningOctagonIcon}
					title={<Trans>Failed to load invites</Trans>}
					description={<Trans>There was an error loading the invite links for this channel. Try again.</Trans>}
					actions={[
						{
							text: i18n._(TRY_AGAIN_DESCRIPTOR),
							onClick: refreshInvites,
							variant: 'primary',
						},
					]}
					fullHeight={true}
					data-flx="channel.channel-tabs.channel-invites-tab.status-slate--2"
				/>
			)}
			<MenuBottomSheet
				isOpen={selectedInvite !== null}
				onClose={handleCloseInviteActions}
				title={selectedInvite ? selectedInvite.code : undefined}
				groups={inviteActionGroups}
				data-flx="channel.channel-tabs.channel-invites-tab.menu-bottom-sheet"
			/>
		</div>
	);
});

export default ChannelInvitesTab;
