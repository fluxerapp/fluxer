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
import {ClipboardIcon, XIcon} from '@phosphor-icons/react';
import {observer} from 'mobx-react-lite';
import React from 'react';
import * as TextCopyActionCreators from '~/actions/TextCopyActionCreators';
import {ChannelTypes} from '~/Constants';
import FocusRing from '~/components/uikit/FocusRing/FocusRing';
import {Tooltip} from '~/components/uikit/Tooltip/Tooltip';
import {useInviteCountdown} from '~/hooks/useInviteCountdown';
import type {Invite} from '~/records/MessageRecord';
import ChannelStore from '~/stores/ChannelStore';
import MobileLayoutStore from '~/stores/MobileLayoutStore';
import RuntimeConfigStore from '~/stores/RuntimeConfigStore';
import UserStore from '~/stores/UserStore';
import {isGuildInvite} from '~/types/InviteTypes';
import * as AvatarUtils from '~/utils/AvatarUtils';
import * as ChannelUtils from '~/utils/ChannelUtils';
import * as DateUtils from '~/utils/DateUtils';
import {stopPropagationOnEnterSpace} from '~/utils/KeyboardUtils';
import styles from './InviteListItem.module.css';

export const InviteListHeader: React.FC<{showChannel?: boolean; showCreatedDate?: boolean}> = observer(
	({showChannel = false, showCreatedDate = false}) => {
		return (
			<div className={showChannel ? styles.header : styles.headerWithoutChannel}>
				<div className={styles.headerColumn}>
					<Trans>Inviter</Trans>
				</div>
				{showChannel && (
					<div className={styles.headerColumn}>
						<Trans>Channel</Trans>
					</div>
				)}
				<div className={styles.headerColumn}>
					<Trans>Code</Trans>
				</div>
				<div className={styles.headerColumn}>
					<Trans>Uses</Trans>
				</div>
				<div className={styles.headerColumn}>{showCreatedDate ? <Trans>Created</Trans> : <Trans>Expires</Trans>}</div>
			</div>
		);
	},
);

export const InviteListItem: React.FC<{
	invite: Invite;
	onRevoke: (code: string) => void;
	showChannel?: boolean;
	showCreatedDate?: boolean;
}> = observer(({invite, onRevoke, showChannel = false, showCreatedDate = false}) => {
	const {t, i18n} = useLingui();
	const {countdown, isMonospace} = useInviteCountdown(invite.expires_at);
	const inviter = UserStore.getUser(invite.inviter?.id || '');
	const avatarUrl = inviter ? AvatarUtils.getUserAvatarURL(inviter, false) : null;
	const {enabled: isMobile} = MobileLayoutStore;

	const guildInvite = isGuildInvite(invite) ? invite : null;
	const channelFromStore = guildInvite ? ChannelStore.getChannel(guildInvite.channel.id) : null;
	const categoryFromStore = channelFromStore ? ChannelStore.getChannel(channelFromStore.parentId || '') : null;

	const channel = showChannel ? channelFromStore : null;
	const category = showChannel && channelFromStore?.parentId ? categoryFromStore : null;

	const usesText = React.useMemo(() => {
		if (!guildInvite) {
			return '0';
		}
		const currentUses = guildInvite.uses ?? 0;
		const maxUses = guildInvite.max_uses ?? 0;
		if (maxUses > 0) {
			return `${currentUses} / ${maxUses}`;
		}
		return String(currentUses);
	}, [guildInvite]);

	const dateDisplay = React.useMemo(() => {
		if (showCreatedDate) {
			if (!guildInvite?.created_at) {
				return '';
			}
			const createdDate = new Date(guildInvite.created_at);
			return DateUtils.getShortRelativeDateString(createdDate) || '';
		}
		return countdown || t`Never`;
	}, [showCreatedDate, guildInvite, countdown]);

	const dateTooltip = React.useMemo(() => {
		if (showCreatedDate) {
			if (!guildInvite?.created_at) {
				return null;
			}
			const createdDate = new Date(guildInvite.created_at);
			return DateUtils.getFormattedDateTimeWithSeconds(createdDate);
		}
		return null;
	}, [showCreatedDate, guildInvite]);

	const dateIsMonospace = !showCreatedDate && isMonospace;

	const handleCopy = (e: React.MouseEvent) => {
		e.stopPropagation();
		TextCopyActionCreators.copy(i18n, `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`);
	};

	const handleRowClick = () => {
		if (isMobile) {
			TextCopyActionCreators.copy(i18n, `${RuntimeConfigStore.inviteEndpoint}/${invite.code}`);
		}
	};

	const handleKeyDown = (e: React.KeyboardEvent) => {
		if (isMobile && (e.key === 'Enter' || e.key === ' ')) {
			e.preventDefault();
			handleRowClick();
		}
	};

	const itemClass = isMobile ? styles.mobileItem : showChannel ? styles.itemWithChannel : styles.itemWithoutChannel;

	return (
		// biome-ignore lint/a11y/noStaticElementInteractions: Click/keyboard handlers only on mobile; hover handled in CSS
		<div
			role={isMobile ? 'button' : undefined}
			onClick={isMobile ? handleRowClick : undefined}
			onKeyDown={isMobile ? handleKeyDown : undefined}
			tabIndex={isMobile ? 0 : undefined}
			className={itemClass}
		>
			<div className={styles.inviter}>
				<span className={styles.label}>
					<Trans>Inviter:</Trans>
				</span>
				{inviter && avatarUrl ? (
					<>
						<img src={avatarUrl} alt="" className={styles.avatar} loading="lazy" />
						<span className={styles.username}>{inviter.username}</span>
					</>
				) : (
					<span className={styles.usernameUnknown}>Unknown</span>
				)}
			</div>

			{showChannel && channel && (
				<div className={styles.channel}>
					<span className={styles.label}>
						<Trans>Channel:</Trans>
					</span>
					{ChannelUtils.getIcon(channel, {size: 20, className: styles.channelIcon})}
					<div className={styles.channelInfo}>
						<span className={styles.channelName}>{channel.name}</span>
						{channel.type !== ChannelTypes.GUILD_CATEGORY && (
							<span className={styles.categoryName}>{category ? category.name : t`No Category`}</span>
						)}
					</div>
				</div>
			)}

			<div className={styles.code}>
				<span className={styles.label}>
					<Trans>Code:</Trans>
				</span>
				<code className={styles.inviteCode}>{invite.code}</code>

				<Tooltip text={t`Click to copy`}>
					<FocusRing offset={-2}>
						<button
							type="button"
							onClick={handleCopy}
							className={styles.copyButtonHidden}
							aria-label={t`Copy invite link`}
							onKeyDown={stopPropagationOnEnterSpace}
						>
							<ClipboardIcon className={styles.copyIcon} />
						</button>
					</FocusRing>
				</Tooltip>
			</div>

			<div className={styles.uses}>
				<span className={styles.label}>
					<Trans>Uses:</Trans>
				</span>
				<span className={styles.usesText}>{usesText}</span>
			</div>

			<div className={styles.date}>
				<span className={styles.label}>{showCreatedDate ? <Trans>Created:</Trans> : <Trans>Expires:</Trans>}</span>
				{dateTooltip ? (
					<Tooltip text={dateTooltip}>
						<span className={dateIsMonospace ? styles.dateTextMonospace : styles.dateText}>{dateDisplay}</span>
					</Tooltip>
				) : (
					<span className={dateIsMonospace ? styles.dateTextMonospace : styles.dateText}>{dateDisplay}</span>
				)}
			</div>

			<Tooltip text={t`Revoke`}>
				<FocusRing offset={-2}>
					<button
						type="button"
						onClick={(e) => {
							e.stopPropagation();
							onRevoke(invite.code);
						}}
						className={styles.revokeButtonHidden}
						aria-label={t`Revoke invite`}
						onKeyDown={stopPropagationOnEnterSpace}
					>
						<XIcon className={styles.revokeIcon} weight="bold" />
					</button>
				</FocusRing>
			</Tooltip>
		</div>
	);
});
