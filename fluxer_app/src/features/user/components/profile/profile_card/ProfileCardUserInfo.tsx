// SPDX-License-Identifier: AGPL-3.0-or-later

import {UserTag} from '@app/features/channel/components/ChannelUserTag';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {useTextOverflow} from '@app/features/ui/hooks/useTextOverflow';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import styles from '@app/features/user/components/profile/profile_card/ProfileCardUserInfo.module.css';
import type {User} from '@app/features/user/models/User';
import * as NicknameUtils from '@app/features/user/utils/NicknameUtils';
import {Trans} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useRef} from 'react';

type DisplayNameStyle = React.CSSProperties & {'--profile-display-name-length': number};

interface ProfileCardUserInfoProps {
	displayName: string;
	displayNameClassName?: string;
	user: User;
	pronouns?: string | null;
	showUsername?: boolean;
	isClickable?: boolean;
	isWebhook?: boolean;
	onDisplayNameClick?: () => void;
	onUsernameClick?: () => void;
	actions?: React.ReactNode;
	usernameActions?: React.ReactNode;
}

export const ProfileCardUserInfo: React.FC<ProfileCardUserInfoProps> = observer(
	({
		displayName,
		displayNameClassName,
		user,
		pronouns,
		showUsername = true,
		isClickable = true,
		isWebhook = false,
		onDisplayNameClick,
		onUsernameClick,
		actions,
		usernameActions,
	}) => {
		const displayNameRef = useRef<HTMLButtonElement>(null);
		const isDisplayNameOverflowing = useTextOverflow(displayNameRef, {
			content: displayName,
			measureTextRange: true,
		});
		const displayNameStyle: DisplayNameStyle = {
			'--profile-display-name-length': Math.max(Array.from(displayName).length, 1),
		};
		const displayNameButton = (
			<button
				ref={displayNameRef}
				type="button"
				onClick={onDisplayNameClick}
				className={clsx(styles.nameButton, displayNameClassName, isClickable && styles.nameButtonClickable)}
				style={displayNameStyle}
				data-flx="user.profile.profile-card.profile-card-user-info.name-button.display-name-click"
			>
				{displayName}
			</button>
		);
		const displayNameContent = isDisplayNameOverflowing ? (
			<Tooltip text={displayName} data-flx="user.profile.profile-card.profile-card-user-info.display-name-tooltip">
				{displayNameButton}
			</Tooltip>
		) : (
			displayNameButton
		);
		return (
			<div
				className={styles.userInfoContainer}
				data-flx="user.profile.profile-card.profile-card-user-info.user-info-container"
			>
				<div className={styles.nameRow} data-flx="user.profile.profile-card.profile-card-user-info.name-row">
					<FocusRing
						offset={-2}
						focusTarget={displayNameRef}
						ringTarget={displayNameRef}
						data-flx="user.profile.profile-card.profile-card-user-info.focus-ring"
					>
						{displayNameContent}
					</FocusRing>
					<div
						className={styles.badgeContainer}
						data-flx="user.profile.profile-card.profile-card-user-info.badge-container"
					>
						{(user.bot || isWebhook) && (
							<UserTag
								className={styles.userTagWrapper}
								system={user.system}
								data-flx="user.profile.profile-card.profile-card-user-info.user-tag-wrapper"
							/>
						)}
					</div>
					{actions && (
						<div
							className={styles.actionsContainer}
							data-flx="user.profile.profile-card.profile-card-user-info.actions-container"
						>
							{actions}
						</div>
					)}
				</div>
				{showUsername && (
					<div className={styles.usernameRow} data-flx="user.profile.profile-card.profile-card-user-info.username-row">
						<FocusRing offset={-2} data-flx="user.profile.profile-card.profile-card-user-info.focus-ring--2">
							<button
								type="button"
								onClick={onUsernameClick}
								className={styles.usernameButton}
								data-flx="user.profile.profile-card.profile-card-user-info.username-button.username-click"
							>
								{NicknameUtils.formatTagForStreamerMode(user.tag)}
							</button>
						</FocusRing>
						{usernameActions}
					</div>
				)}
				{pronouns && (
					<div className={styles.pronouns} data-flx="user.profile.profile-card.profile-card-user-info.pronouns">
						<span className={styles.srOnly} data-flx="user.profile.profile-card.profile-card-user-info.sr-only">
							<Trans>Pronouns: </Trans>
						</span>
						{pronouns}
					</div>
				)}
			</div>
		);
	},
);
