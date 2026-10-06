// SPDX-License-Identifier: AGPL-3.0-or-later

import {ConfirmModal} from '@app/features/app/components/dialogs/ConfirmModal';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import styles from '@app/features/channel/components/modals/GroupInvitesBottomSheet.module.css';
import {GO_BACK_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import * as InviteCommands from '@app/features/invite/commands/InviteCommands';
import {InviteRevokeFailedModal} from '@app/features/invite/components/alerts/InviteRevokeFailedModal';
import {InvitesLoadFailedModal} from '@app/features/invite/components/alerts/InvitesLoadFailedModal';
import {Logger} from '@app/features/platform/utils/AppLogger';
import StreamerMode from '@app/features/streamer_mode/state/StreamerMode';
import {BottomSheet} from '@app/features/ui/bottom_sheet/BottomSheet';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import * as ToastCommands from '@app/features/ui/commands/ToastCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import {Scroller} from '@app/features/ui/components/Scroller';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import Users from '@app/features/user/state/Users';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import type {Invite} from '@fluxer/schema/src/domains/invite/InviteSchemas';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {ArrowLeftIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useEffect, useState} from 'react';

const REVOKE_INVITE_DESCRIPTOR = msg({
	message: 'Revoke invite',
	comment: 'Short label in the group invites bottom sheet. Keep it concise.',
});
const ARE_YOU_SURE_YOU_WANT_TO_REVOKE_THIS_DESCRIPTOR = msg({
	message: "Revoke this invite? Can't be undone.",
	comment: 'Error message in the group invites bottom sheet.',
});
const REVOKE_DESCRIPTOR = msg({
	message: 'Revoke',
	comment: 'Short label in the group invites bottom sheet. Keep it concise.',
});
const EXPIRED_INVITE_METADATA_DESCRIPTOR = msg({
	message: 'Created by {creatorDisplayName}. Expired.',
	comment: 'Error message in the group invites bottom sheet.',
});
const GROUP_INVITES_DESCRIPTOR = msg({
	message: 'Group invites',
	comment: 'Short label in the group invites bottom sheet. Keep it concise.',
});
const LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR = msg({
	message: 'Link hidden while sharing',
	comment: 'Replacement text for an invite URL while streaming privacy is active.',
});
const UNKNOWN_USER_DESCRIPTOR = msg({message: 'Unknown user'});
const NEVER_INVITE_METADATA_DESCRIPTOR = msg({message: 'Created by {creatorDisplayName}. Never expires.'});
const DAYS_INVITE_METADATA_DESCRIPTOR = msg({
	message:
		'{days, plural, one {Created by {creatorDisplayName}. Expires in # day.} other {Created by {creatorDisplayName}. Expires in # days.}}',
});
const HOURS_INVITE_METADATA_DESCRIPTOR = msg({
	message:
		'{hours, plural, =0 {Created by {creatorDisplayName}. Expires in less than an hour.} one {Created by {creatorDisplayName}. Expires in # hour.} other {Created by {creatorDisplayName}. Expires in # hours.}}',
});
const logger = new Logger('GroupInvitesBottomSheet');

interface GroupInvitesBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
}

export const GroupInvitesBottomSheet: React.FC<GroupInvitesBottomSheetProps> = observer(
	({isOpen, onClose, channelId}) => {
		const {i18n} = useLingui();
		const [invites, setInvites] = useState<Array<Invite> | null>(null);
		const [isLoading, setIsLoading] = useState(true);
		const hideInviteLinks = StreamerMode.shouldHideInviteLinks;
		const loadInvites = useCallback(async () => {
			try {
				setIsLoading(true);
				const data = await InviteCommands.list(channelId);
				setInvites(data);
			} catch (error) {
				logger.error('Failed to load invites:', error);
				ModalCommands.push(
					modal(() => (
						<InvitesLoadFailedModal data-flx="channel.group-invites-bottom-sheet.load-invites.invites-load-failed-modal" />
					)),
				);
			} finally {
				setIsLoading(false);
			}
		}, [channelId]);
		useEffect(() => {
			if (isOpen) {
				loadInvites();
			}
		}, [isOpen, loadInvites]);
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
										ModalCommands.push(
											modal(() => (
												<InviteRevokeFailedModal data-flx="channel.group-invites-bottom-sheet.handle-revoke.invite-revoke-failed-modal" />
											)),
										);
									}, 0);
								}
							}}
							data-flx="channel.group-invites-bottom-sheet.handle-revoke.confirm-modal"
						/>
					)),
				);
			},
			[loadInvites],
		);
		const formatInviteMetadata = (expiresAt: string | null, creatorDisplayName: string) => {
			if (expiresAt === null) return i18n._(NEVER_INVITE_METADATA_DESCRIPTOR, {creatorDisplayName});
			const millisecondsRemaining = new Date(expiresAt).getTime() - Date.now();
			if (millisecondsRemaining <= 0) return i18n._(EXPIRED_INVITE_METADATA_DESCRIPTOR, {creatorDisplayName});
			const hours = Math.floor(millisecondsRemaining / (1000 * 60 * 60));
			const days = Math.floor(hours / 24);
			if (days > 0) return i18n._(DAYS_INVITE_METADATA_DESCRIPTOR, {creatorDisplayName, days});
			return i18n._(HOURS_INVITE_METADATA_DESCRIPTOR, {creatorDisplayName, hours});
		};
		return (
			<BottomSheet
				isOpen={isOpen}
				onClose={onClose}
				snapPoints={[0, 1]}
				initialSnap={1}
				disablePadding={true}
				surface="primary"
				leadingAction={
					<button
						type="button"
						onClick={onClose}
						className={styles.backButton}
						aria-label={i18n._(GO_BACK_DESCRIPTOR)}
						data-flx="channel.group-invites-bottom-sheet.back-button.close"
					>
						<ArrowLeftIcon
							className={styles.backIcon}
							weight="bold"
							data-flx="channel.group-invites-bottom-sheet.back-icon"
						/>
					</button>
				}
				title={i18n._(GROUP_INVITES_DESCRIPTOR)}
				data-flx="channel.group-invites-bottom-sheet.bottom-sheet"
			>
				<div className={styles.container} data-flx="channel.group-invites-bottom-sheet.container">
					<Scroller
						className={styles.scroller}
						key="group-invites-bottom-sheet-scroller"
						data-flx="channel.group-invites-bottom-sheet.scroller"
					>
						<div className={styles.content} data-flx="channel.group-invites-bottom-sheet.content">
							{isLoading ? (
								<div
									className={styles.loadingContainer}
									data-flx="channel.group-invites-bottom-sheet.loading-container"
								>
									<p className={styles.loadingText} data-flx="channel.group-invites-bottom-sheet.loading-text">
										<Trans>Loading invites...</Trans>
									</p>
								</div>
							) : invites && invites.length === 0 ? (
								<div className={styles.emptyContainer} data-flx="channel.group-invites-bottom-sheet.empty-container">
									<p className={styles.emptyText} data-flx="channel.group-invites-bottom-sheet.empty-text">
										<Trans>No invites created</Trans>
									</p>
								</div>
							) : (
								<div className={styles.inviteList} data-flx="channel.group-invites-bottom-sheet.invite-list">
									{invites?.map((invite) => {
										const inviter = invite.inviter ? Users.getUser(invite.inviter.id) : null;
										const creatorDisplayName = inviter
											? NicknameUtils.getNickname(inviter, undefined, channelId)
											: invite.inviter
												? NicknameUtils.getDisplayName(invite.inviter)
												: i18n._(UNKNOWN_USER_DESCRIPTOR);
										return (
											<div
												key={invite.code}
												className={styles.inviteItem}
												data-flx="channel.group-invites-bottom-sheet.invite-item"
											>
												{inviter && (
													<Avatar user={inviter} size={32} data-flx="channel.group-invites-bottom-sheet.avatar" />
												)}
												<div
													className={styles.inviteDetails}
													data-flx="channel.group-invites-bottom-sheet.invite-details"
												>
													<div className={styles.inviteUrl} data-flx="channel.group-invites-bottom-sheet.invite-url">
														{hideInviteLinks
															? i18n._(LINK_HIDDEN_WHILE_SHARING_DESCRIPTOR)
															: `${RuntimeConfig.inviteEndpoint}/${invite.code}`}
													</div>
													<div className={styles.inviteInfo} data-flx="channel.group-invites-bottom-sheet.invite-info">
														{formatInviteMetadata(invite.expires_at, creatorDisplayName)}
													</div>
												</div>
												<Tooltip
													text={i18n._(REVOKE_INVITE_DESCRIPTOR)}
													data-flx="channel.group-invites-bottom-sheet.tooltip"
												>
													<button
														type="button"
														onClick={() => handleRevoke(invite.code)}
														className={styles.revokeButton}
														aria-label={i18n._(REVOKE_INVITE_DESCRIPTOR)}
														data-flx="channel.group-invites-bottom-sheet.revoke-button"
													>
														<TrashIcon
															className={styles.revokeIcon}
															data-flx="channel.group-invites-bottom-sheet.revoke-icon"
														/>
													</button>
												</Tooltip>
											</div>
										);
									})}
								</div>
							)}
						</div>
					</Scroller>
				</div>
			</BottomSheet>
		);
	},
);
