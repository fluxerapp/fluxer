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
import {ArrowLeftIcon, TrashIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as InviteActionCreators from '~/actions/InviteActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import {modal} from '~/actions/ModalActionCreators';
import * as ToastActionCreators from '~/actions/ToastActionCreators';
import {InviteRevokeFailedModal} from '~/components/alerts/InviteRevokeFailedModal';
import {InvitesLoadFailedModal} from '~/components/alerts/InvitesLoadFailedModal';
import {ConfirmModal} from '~/components/modals/ConfirmModal';
import {Avatar} from '~/components/uikit/Avatar';
import {BottomSheet} from '~/components/uikit/BottomSheet/BottomSheet';
import {Scroller} from '~/components/uikit/Scroller';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import type {Invite} from '~/records/MessageRecord';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import UserStore from '~/stores/UserStore';
import styles from './GroupInvitesBottomSheet.module.css';

interface GroupInvitesBottomSheetProps {
	isOpen: boolean;
	onClose: () => void;
	channelId: string;
}

export const GroupInvitesBottomSheet: React.FC<GroupInvitesBottomSheetProps> = observer(
	({isOpen, onClose, channelId}) => {
		const {t} = useLingui();
		const [invites, setInvites] = React.useState<Array<Invite> | null>(null);
		const [isLoading, setIsLoading] = React.useState(true);

		const loadInvites = React.useCallback(async () => {
			try {
				setIsLoading(true);
				const data = await InviteActionCreators.list(channelId);
				setInvites(data);
			} catch (error) {
				console.error('Failed to load invites:', error);
				ModalActionCreators.push(modal(() => <InvitesLoadFailedModal />));
			} finally {
				setIsLoading(false);
			}
		}, [channelId]);

		React.useEffect(() => {
			if (isOpen) {
				loadInvites();
			}
		}, [isOpen, loadInvites]);

		const handleRevoke = React.useCallback(
			(code: string) => {
				ModalActionCreators.push(
					modal(() => (
						<ConfirmModal
							title={t`Revoke invite`}
							description={t`Are you sure you want to revoke this invite? This action cannot be undone.`}
							primaryText={t`Revoke`}
							onPrimary={async () => {
								try {
									await InviteActionCreators.remove(code);
									ToastActionCreators.createToast({
										type: 'success',
										children: <Trans>Invite revoked</Trans>,
									});
									await loadInvites();
								} catch (error) {
									console.error('Failed to revoke invite:', error);
									ModalActionCreators.push(modal(() => <InviteRevokeFailedModal />));
								}
							}}
						/>
					)),
				);
			},
			[loadInvites],
		);

		const formatExpiresAt = (expiresAt: string | null) => {
			if (!expiresAt) return t`Never`;
			const date = new Date(expiresAt);
			const now = new Date();
			const diff = date.getTime() - now.getTime();
			if (diff < 0) return t`Expired`;
			const hours = Math.floor(diff / (1000 * 60 * 60));
			const days = Math.floor(hours / 24);
			if (days > 0) return t`${days} days`;
			return t`${hours} hours`;
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
					<button type="button" onClick={onClose} className={styles.backButton}>
						<ArrowLeftIcon className={styles.backIcon} weight="bold" />
					</button>
				}
				title={t`Group Invites`}
			>
				<div className={styles.container}>
					<Scroller className={styles.scroller} key="group-invites-bottom-sheet-scroller">
						<div className={styles.content}>
							{isLoading ? (
								<div className={styles.loadingContainer}>
									<p className={styles.loadingText}>
										<Trans>Loading invites...</Trans>
									</p>
								</div>
							) : invites && invites.length === 0 ? (
								<div className={styles.emptyContainer}>
									<p className={styles.emptyText}>
										<Trans>No invites created</Trans>
									</p>
								</div>
							) : (
								<div className={styles.inviteList}>
									{invites?.map((invite) => {
										const inviter = invite.inviter ? UserStore.getUser(invite.inviter.id) : null;
										return (
											<div key={invite.code} className={styles.inviteItem}>
												{inviter && <Avatar user={inviter} size={32} />}
												<div className={styles.inviteDetails}>
													<div className={styles.inviteUrl}>
														{RuntimeConfigStore.inviteEndpoint}/{invite.code}
													</div>
													<div className={styles.inviteInfo}>
														<Trans>
															Created by {inviter?.username}. Expires in {formatExpiresAt(invite.expires_at)}.
														</Trans>
													</div>
												</div>
												<Tooltip text={t`Revoke invite`}>
													<button
														type="button"
														onClick={() => handleRevoke(invite.code)}
														className={styles.revokeButton}
														aria-label={t`Revoke invite`}
													>
														<TrashIcon className={styles.revokeIcon} />
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
