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

import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import * as UserProfileActionCreators from '~/actions/UserProfileActionCreators';
import {Avatar} from '~/components/uikit/Avatar';
import type {UserRecord} from '~/records/UserRecord';
import styles from './GuildAuditLogTab.module.css';

interface UserTagParts {
	name: string;
	discriminator: string | null;
}

const splitTag = (tag: string): UserTagParts => {
	const idx = tag.lastIndexOf('#');
	if (idx <= 0) return {name: tag, discriminator: null};
	return {name: tag.slice(0, idx), discriminator: tag.slice(idx + 1)};
};

export const ColorDot: React.FC<{color: string; className?: string}> = ({color, className}) => (
	<span className={clsx(styles.colorHook, className)} style={{backgroundColor: color}} aria-hidden />
);

export const InlineCode: React.FC<{children: React.ReactNode; className?: string; title?: string}> = ({
	children,
	className,
	title,
}) => (
	<span className={clsx(styles.inlineCode, className)} title={title}>
		{children}
	</span>
);

export const UserHook: React.FC<{user: UserRecord; className?: string}> = ({user, className}) => {
	const parts = splitTag(user.tag);
	return (
		<span className={clsx(styles.userHook, className)}>
			<span className={styles.userName}>{user.displayName ?? parts.name}</span>
			{parts.discriminator ? <span className={styles.discrim}>#{parts.discriminator}</span> : null}
		</span>
	);
};

export const TargetHook: React.FC<{label: string; className?: string; title?: string}> = ({
	label,
	className,
	title,
}) => (
	<strong className={clsx(styles.targetHook, className)} title={title}>
		{label}
	</strong>
);

interface ClickableUserProps {
	user: UserRecord;
	guildId?: string;
	className?: string;
	showAvatar?: boolean;
}

export const ClickableUser: React.FC<ClickableUserProps> = observer(({user, guildId, className, showAvatar = true}) => {
	const handleClick = () => {
		UserProfileActionCreators.openUserProfile(user.id, guildId);
	};

	const handleKeyDown = (event: React.KeyboardEvent) => {
		if (event.key === 'Enter' || event.key === ' ') {
			event.preventDefault();
			handleClick();
		}
	};

	return (
		<span
			className={clsx(styles.clickableUser, className)}
			onClick={handleClick}
			onKeyDown={handleKeyDown}
			role="button"
			tabIndex={0}
		>
			{showAvatar ? <Avatar user={user} size={16} guildId={guildId} /> : null}
			<span className={styles.clickableUserName}>{user.displayName}</span>
		</span>
	);
});
