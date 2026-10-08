// SPDX-License-Identifier: AGPL-3.0-or-later

import * as Modal from '@app/features/app/components/dialogs/Modal';
import type {GuildBan} from '@app/features/guild/commands/GuildCommands';
import {CLOSE_DESCRIPTOR, EXPIRES_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import styles from '@app/features/moderation/components/modals/BanDetailsModal.module.css';
import {Button} from '@app/features/ui/button/Button';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import Users from '@app/features/user/state/Users';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as DateUtils from '@app/features/user/utils/DateFormatting';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {formatUserTag} from '@app/features/user/utils/UserTagUtils';
import {msg} from '@lingui/core/macro';
import {Trans, useLingui} from '@lingui/react/macro';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback, useState} from 'react';

const BAN_DETAILS_DESCRIPTOR = msg({
	message: 'Ban details',
	comment: 'Button or menu action label in the ban details modal. Keep it concise. Keep the tone plain and specific.',
});

interface BanDetailsModalProps {
	ban: GuildBan;
	onRevoke?: () => void;
}

export const BanDetailsModal: React.FC<BanDetailsModalProps> = observer(({ban, onRevoke}) => {
	const {i18n} = useLingui();
	const moderator = Users.getUser(ban.moderator_id);
	const avatarUrl = AvatarUtils.getUserAvatarURL(ban.user, false);
	const [isRevoking, setIsRevoking] = useState(false);
	const userDisplayName = NicknameUtils.getDisplayName(ban.user);
	const userTag = NicknameUtils.formatTagForStreamerMode(
		ban.user.tag ?? formatUserTag({...ban.user, discriminator: (ban.user.discriminator ?? '').padStart(4, '0')}),
	);
	const handleRevoke = useCallback(async () => {
		if (!onRevoke) return;
		setIsRevoking(true);
		try {
			await onRevoke();
		} finally {
			setIsRevoking(false);
		}
		ModalCommands.popByType(BanDetailsModal);
	}, [onRevoke]);
	return (
		<Modal.Root size="small" centered data-flx="moderation.ban-details-modal.modal-root">
			<Modal.Header title={i18n._(BAN_DETAILS_DESCRIPTOR)} data-flx="moderation.ban-details-modal.modal-header" />
			<Modal.Content data-flx="moderation.ban-details-modal.modal-content">
				<Modal.ContentLayout data-flx="moderation.ban-details-modal.modal-content-layout">
					<div className={styles.userSection} data-flx="moderation.ban-details-modal.user-section">
						{avatarUrl ? (
							<img src={avatarUrl} alt="" className={styles.avatar} data-flx="moderation.ban-details-modal.avatar" />
						) : (
							<div className={styles.avatarPlaceholder} data-flx="moderation.ban-details-modal.avatar-placeholder">
								{userDisplayName[0].toUpperCase()}
							</div>
						)}
						<div className={styles.userInfo} data-flx="moderation.ban-details-modal.user-info">
							<span className={styles.username} data-flx="moderation.ban-details-modal.username">
								{userDisplayName}
							</span>
							<span className={styles.tag} data-flx="moderation.ban-details-modal.tag">
								{userTag}
							</span>
						</div>
					</div>
					<div className={styles.details} data-flx="moderation.ban-details-modal.details">
						<div className={styles.detailRow} data-flx="moderation.ban-details-modal.detail-row">
							<span className={styles.detailLabel} data-flx="moderation.ban-details-modal.detail-label">
								<Trans>Reason</Trans>
							</span>
							<span className={styles.detailValue} data-flx="moderation.ban-details-modal.detail-value">
								{ban.reason || (
									<span className={styles.noReason} data-flx="moderation.ban-details-modal.no-reason">
										<Trans>No reason provided</Trans>
									</span>
								)}
							</span>
						</div>
						<div className={styles.detailRow} data-flx="moderation.ban-details-modal.detail-row--2">
							<span className={styles.detailLabel} data-flx="moderation.ban-details-modal.detail-label--2">
								<Trans>Banned on</Trans>
							</span>
							<span className={styles.detailValue} data-flx="moderation.ban-details-modal.detail-value--2">
								{DateUtils.getFormattedShortDate(new Date(ban.banned_at))}
							</span>
						</div>
						{ban.expires_at && (
							<div className={styles.detailRow} data-flx="moderation.ban-details-modal.detail-row--3">
								<span className={styles.detailLabel} data-flx="moderation.ban-details-modal.detail-label--3">
									{i18n._(EXPIRES_DESCRIPTOR)}
								</span>
								<span className={styles.detailValue} data-flx="moderation.ban-details-modal.detail-value--3">
									{DateUtils.getFormattedShortDate(new Date(ban.expires_at))}
								</span>
							</div>
						)}
						<div className={styles.detailRow} data-flx="moderation.ban-details-modal.detail-row--4">
							<span className={styles.detailLabel} data-flx="moderation.ban-details-modal.detail-label--4">
								<Trans>Banned by</Trans>
							</span>
							<span className={styles.detailValue} data-flx="moderation.ban-details-modal.detail-value--4">
								{moderator ? (
									<span className={styles.moderator} data-flx="moderation.ban-details-modal.moderator">
										<Avatar user={moderator} size={20} data-flx="moderation.ban-details-modal.avatar--2" />
										<span data-flx="moderation.ban-details-modal.span">{NicknameUtils.getDisplayName(moderator)}</span>
									</span>
								) : (
									<span className={styles.unknownModerator} data-flx="moderation.ban-details-modal.unknown-moderator">
										<Trans>Unknown</Trans>
									</span>
								)}
							</span>
						</div>
					</div>
				</Modal.ContentLayout>
			</Modal.Content>
			{onRevoke && (
				<Modal.Footer data-flx="moderation.ban-details-modal.modal-footer">
					<Button
						variant="secondary"
						onClick={() => ModalCommands.pop()}
						data-flx="moderation.ban-details-modal.button.pop"
					>
						{i18n._(CLOSE_DESCRIPTOR)}
					</Button>
					<Button
						variant="danger"
						submitting={isRevoking}
						onClick={handleRevoke}
						data-flx="moderation.ban-details-modal.button.revoke"
					>
						<Trans>Revoke ban</Trans>
					</Button>
				</Modal.Footer>
			)}
		</Modal.Root>
	);
});
