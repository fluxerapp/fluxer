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
import React from 'react';
import type {GuildBan} from '~/actions/GuildActionCreators';
import * as ModalActionCreators from '~/actions/ModalActionCreators';
import * as Modal from '~/components/modals/Modal';
import {Avatar} from '~/components/uikit/Avatar';
import {Button} from '~/components/uikit/Button/Button';
import UserStore from '~/stores/UserStore';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as DateUtils from '~/utils/DateUtils';
import styles from './BanDetailsModal.module.css';

interface BanDetailsModalProps {
	ban: GuildBan;
	onRevoke?: () => void;
}

export const BanDetailsModal: React.FC<BanDetailsModalProps> = observer(({ban, onRevoke}) => {
	const {t} = useLingui();
	const moderator = UserStore.getUser(ban.moderator_id);
	const avatarUrl = AvatarUtils.getUserAvatarURL(ban.user, false);
	const [isRevoking, setIsRevoking] = React.useState(false);
	const userTag = ban.user.tag ?? `${ban.user.username}#${(ban.user.discriminator ?? '').padStart(4, '0')}`;

	const handleRevoke = React.useCallback(async () => {
		if (!onRevoke) return;
		setIsRevoking(true);
		try {
			await onRevoke();
			ModalActionCreators.pop();
		} finally {
			setIsRevoking(false);
		}
	}, [onRevoke]);

	return (
		<Modal.Root size="small" centered>
			<Modal.Header title={t`Ban Details`} />
			<Modal.Content>
				<div className={styles.container}>
					<div className={styles.userSection}>
						{avatarUrl ? (
							<img src={avatarUrl} alt="" className={styles.avatar} />
						) : (
							<div className={styles.avatarPlaceholder}>{ban.user.username[0].toUpperCase()}</div>
						)}
						<div className={styles.userInfo}>
							<span className={styles.username}>{ban.user.username}</span>
							<span className={styles.tag}>{userTag}</span>
						</div>
					</div>

					<div className={styles.details}>
						<div className={styles.detailRow}>
							<span className={styles.detailLabel}>
								<Trans>Reason</Trans>
							</span>
							<span className={styles.detailValue}>
								{ban.reason || (
									<span className={styles.noReason}>
										<Trans>No reason provided</Trans>
									</span>
								)}
							</span>
						</div>

						<div className={styles.detailRow}>
							<span className={styles.detailLabel}>
								<Trans>Banned on</Trans>
							</span>
							<span className={styles.detailValue}>{DateUtils.getFormattedShortDate(new Date(ban.banned_at))}</span>
						</div>

						{ban.expires_at && (
							<div className={styles.detailRow}>
								<span className={styles.detailLabel}>
									<Trans>Expires</Trans>
								</span>
								<span className={styles.detailValue}>{DateUtils.getFormattedShortDate(new Date(ban.expires_at))}</span>
							</div>
						)}

						<div className={styles.detailRow}>
							<span className={styles.detailLabel}>
								<Trans>Banned by</Trans>
							</span>
							<span className={styles.detailValue}>
								{moderator ? (
									<span className={styles.moderator}>
										<Avatar user={moderator} size={20} />
										<span>{moderator.username}</span>
									</span>
								) : (
									<span className={styles.unknownModerator}>
										<Trans>Unknown</Trans>
									</span>
								)}
							</span>
						</div>
					</div>
				</div>
			</Modal.Content>
			<Modal.Footer>
				<Button variant="secondary" onClick={() => ModalActionCreators.pop()}>
					<Trans>Close</Trans>
				</Button>
				{onRevoke && (
					<Button variant="danger-primary" submitting={isRevoking} onClick={handleRevoke}>
						<Trans>Revoke Ban</Trans>
					</Button>
				)}
			</Modal.Footer>
		</Modal.Root>
	);
});
