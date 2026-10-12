// SPDX-License-Identifier: AGPL-3.0-or-later

import modalI18n from '@app/app/I18n';
import {showGenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModalCommands';
import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import * as Modal from '@app/features/app/components/dialogs/Modal';
import Authentication from '@app/features/auth/state/Authentication';
import styles from '@app/features/channel/components/modals/GroupInvitesModal.module.css';
import Channels from '@app/features/channel/state/Channels';
import {TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {InviteDateToggle} from '@app/features/invite/components/InviteDateToggle';
import {InviteListHeader, InviteListItem} from '@app/features/invite/components/InviteListItem';
import {Logger} from '@app/features/platform/utils/AppLogger';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Spinner} from '@app/features/ui/components/Spinner';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import {useCallback, useEffect, useState} from 'react';

const InviteRevokeFailedModal_FAILED_TO_REVOKE_INVITE_DESCRIPTOR = msg({
	message: "Couldn't revoke invite",
	comment: 'Error modal title shown when revoking an invite fails.',
});

const InviteRevokeFailedModal_WE_COULDN_T_REVOKE_THE_INVITE_THE_LINK_DESCRIPTOR = msg({
	message: 'The link may still work. Try again in a moment.',
	comment: 'Error modal body shown when revoking an invite fails; warns the user that the link may still be live.',
});

const InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR = msg({
	message: "Couldn't load invites",
	comment: 'Error modal title shown when fetching invites fails.',
});

const REVOKE_INVITE_DESCRIPTOR = msg({
	message: 'Revoke invite',
	comment: 'Short label in the group invites modal. Keep it concise.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REVOKE_THIS_DESCRIPTOR = msg({
	message: "Revoke this invite? Can't be undone.",
	comment: 'Error message in the group invites modal.',
});
const REVOKE_DESCRIPTOR = msg({
	message: 'Revoke',
	comment: 'Short label in the group invites modal. Keep it concise.',
});
const GROUP_INVITES_DESCRIPTOR = msg({
	message: 'Group invites',
	comment: 'Short label in the group invites modal. Keep it concise.',
});
const logger = new Logger('GroupInvitesModal');
export const GroupInvitesModal = observer(({channelId}: {channelId: string}) => {
	const {i18n} = useLingui();
	const channel = Channels.getChannel(channelId);
	const isOwner = channel?.ownerId === Authentication.currentUserId;
	const [invites, setInvites] = useState<Array<Invite>>([]);
	const [fetchStatus, setFetchStatus] = useState<'idle' | 'pending' | 'success' | 'error'>('idle');
	const [showCreatedDate, setShowCreatedDate] = useState(false);
	const loadInvites = useCallback(async () => {
		if (!isOwner) return;
		try {
			setFetchStatus('pending');
			const data = await InviteCommands.list(channelId);
			setInvites(data);
			setFetchStatus('success');
		} catch (error) {
			logger.error('Failed to load invites:', error);
			setFetchStatus('error');
			showGenericErrorModal({
				title: () => modalI18n._(InvitesLoadFailedModal_FAILED_TO_LOAD_INVITES_DESCRIPTOR),
				message: () => modalI18n._(TRY_AGAIN_IN_A_MOMENT_DESCRIPTOR),
				dataFlx: 'invite.invites-load-failed-modal.generic-error-modal',
			});
		}
	}, [channelId, isOwner]);
	useEffect(() => {
		if (isOwner && fetchStatus === 'idle') {
			loadInvites();
		}
	}, [fetchStatus, loadInvites, isOwner]);
	const handleRevoke = useCallback(
		(code: string) => {
			ModalCommands.push(
				modal(() => (
					<ConfirmModal
						title={i18n._(REVOKE_INVITE_DESCRIPTOR)}
						description={i18n._(ARE_YOU_SURE_YOU_WANT_TO_REVOKE_THIS_DESCRIPTOR)}
						primaryText={i18n._(REVOKE_DESCRIPTOR)}
						onPrimary={async () => {
							try {
								await InviteCommands.remove(code);
								ToastCommands.createToast({
									type: 'success',
									children: <Trans>Invite revoked</Trans>,
								});
								await loadInvites();
							} catch (error) {
								logger.error('Failed to revoke invite:', error);
								window.setTimeout(() => {
									showGenericErrorModal({
										title: () => modalI18n._(InviteRevokeFailedModal_FAILED_TO_REVOKE_INVITE_DESCRIPTOR),
										message: () =>
											modalI18n._(InviteRevokeFailedModal_WE_COULDN_T_REVOKE_THE_INVITE_THE_LINK_DESCRIPTOR),
										dataFlx: 'invite.invite-revoke-failed-modal.generic-error-modal',
									});
								}, 0);
							}
						}}
						data-flx="channel.group-invites-modal.handle-revoke.confirm-modal"
					/>
				)),
			);
		},
		[loadInvites],
	);
	if (!isOwner) {
		return (
			<Modal.Root className={styles.modalRoot} data-flx="channel.group-invites-modal.modal-root">
				<Modal.Header title={i18n._(GROUP_INVITES_DESCRIPTOR)} data-flx="channel.group-invites-modal.modal-header" />
				<Modal.Content data-flx="channel.group-invites-modal.modal-content">
					<div className={styles.container} data-flx="channel.group-invites-modal.container">
						<div className={styles.errorBox} data-flx="channel.group-invites-modal.error-box">
							<p className={styles.errorText} data-flx="channel.group-invites-modal.error-text">
								<Trans>Only the group owner can manage invites.</Trans>
							</p>
						</div>
					</div>
				</Modal.Content>
			</Modal.Root>
		);
	}
	return (
		<Modal.Root className={styles.modalRoot} data-flx="channel.group-invites-modal.modal-root--2">
			<Modal.Header title={i18n._(GROUP_INVITES_DESCRIPTOR)} data-flx="channel.group-invites-modal.modal-header--2" />
			<Modal.Content data-flx="channel.group-invites-modal.modal-content--2">
				<div className={styles.container} data-flx="channel.group-invites-modal.container--2">
					{fetchStatus === 'pending' && (
						<div className={styles.spinnerContainer} data-flx="channel.group-invites-modal.spinner-container">
							<Spinner data-flx="channel.group-invites-modal.spinner" />
						</div>
					)}
					{fetchStatus === 'error' && (
						<div className={styles.errorBox} data-flx="channel.group-invites-modal.error-box--2">
							<p className={styles.errorText} data-flx="channel.group-invites-modal.error-text--2">
								<Trans>Failed to load invites. Try again.</Trans>
							</p>
						</div>
					)}
					{fetchStatus === 'success' && invites.length === 0 && (
						<div className={styles.stateBox} data-flx="channel.group-invites-modal.state-box">
							<p className={styles.stateText} data-flx="channel.group-invites-modal.state-text">
								<Trans>No invites created</Trans>
							</p>
						</div>
					)}
					{fetchStatus === 'success' && invites.length > 0 && (
						<div className={styles.invitesWrapper} data-flx="channel.group-invites-modal.invites-wrapper">
							<InviteDateToggle
								showCreatedDate={showCreatedDate}
								onToggle={setShowCreatedDate}
								data-flx="channel.group-invites-modal.invite-date-toggle"
							/>
							<div className={styles.invitesList} data-flx="channel.group-invites-modal.invites-list">
								<Scroller
									className={styles.scroller}
									key="group-invites-scroller"
									data-flx="channel.group-invites-modal.scroller"
								>
									<InviteListHeader
										showCreatedDate={showCreatedDate}
										data-flx="channel.group-invites-modal.invite-list-header"
									/>
									<div className={styles.inviteItems} data-flx="channel.group-invites-modal.invite-items">
										{invites.map((invite) => (
											<InviteListItem
												key={invite.code}
												invite={invite}
												onRevoke={handleRevoke}
												showCreatedDate={showCreatedDate}
												data-flx="channel.group-invites-modal.invite-list-item"
											/>
										))}
									</div>
								</Scroller>
							</div>
						</div>
					)}
				</div>
			</Modal.Content>
		</Modal.Root>
	);
});
