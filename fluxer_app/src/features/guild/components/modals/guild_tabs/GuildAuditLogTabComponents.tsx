// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/guild/components/modals/guild_tabs/GuildAuditLogTab.module.css';
import {isKeyboardActivationKey} from '@app/features/input/utils/KeyboardUtils';
import * as TextCopyCommands from '@app/features/ui/commands/TextCopyCommands';
import {Avatar} from '@app/features/ui/components/Avatar';
import FocusRing from '@app/features/ui/focus_ring/FocusRing';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import * as UserProfileCommands from '@app/features/user/commands/UserProfileCommands';
import type {User} from '@app/features/user/models/User';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import clsx from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useCallback} from 'react';

const CLICK_TO_COPY_ID_DESCRIPTOR = msg({
	message: 'Click to copy ID',
	comment: 'Label in the guild audit log tab.components.',
});

export const ColorDot: React.FC<{color: string; className?: string}> = ({color, className}) => (
	<span
		className={clsx(styles.colorHook, className)}
		style={{backgroundColor: color}}
		aria-hidden
		data-flx="guild.guild-tabs.guild-audit-log-tab-components.color-dot.color-hook"
	/>
);
export const InlineCode: React.FC<{children: React.ReactNode; className?: string; title?: string}> = ({
	children,
	className,
	title,
}) => {
	const content = (
		<span
			className={clsx(styles.inlineCode, className)}
			data-flx="guild.guild-tabs.guild-audit-log-tab-components.inline-code.inline-code"
		>
			{children}
		</span>
	);
	return title ? (
		<Tooltip text={title} data-flx="guild.guild-tabs.guild-audit-log-tab-components.inline-code.tooltip">
			{content}
		</Tooltip>
	) : (
		content
	);
};

interface ClickableUserProps {
	user: User;
	guildId?: string;
	className?: string;
	showAvatar?: boolean;
}

export const ClickableUser: React.FC<ClickableUserProps> = observer(({user, guildId, className, showAvatar = true}) => {
	const handleClick = (event: React.MouseEvent) => {
		event.stopPropagation();
		UserProfileCommands.openUserProfile(user.id, guildId);
	};
	const handleKeyDown = (event: React.KeyboardEvent) => {
		event.stopPropagation();
		if (isKeyboardActivationKey(event.key)) {
			event.preventDefault();
			UserProfileCommands.openUserProfile(user.id, guildId);
		}
	};
	return (
		<FocusRing offset={-2} data-flx="guild.guild-tabs.guild-audit-log-tab-components.clickable-user.focus-ring">
			<span
				className={clsx(styles.clickableUser, className)}
				onClick={handleClick}
				onKeyDown={handleKeyDown}
				role="button"
				tabIndex={0}
				data-flx="guild.guild-tabs.guild-audit-log-tab-components.clickable-user.clickable-user"
			>
				{showAvatar ? (
					<Avatar
						user={user}
						size={16}
						guildId={guildId}
						data-flx="guild.guild-tabs.guild-audit-log-tab-components.clickable-user.avatar"
					/>
				) : null}
				<span
					className={styles.clickableUserName}
					data-flx="guild.guild-tabs.guild-audit-log-tab-components.clickable-user.clickable-user-name"
				>
					{user.displayName}
				</span>
			</span>
		</FocusRing>
	);
});

interface CopyIdInlineProps {
	id: string;
	children: React.ReactNode;
	className?: string;
}

export const CopyIdInline: React.FC<CopyIdInlineProps> = ({id, children, className}) => {
	const {i18n} = useLingui();
	const copyId = useCallback(() => {
		void TextCopyCommands.copy(i18n, id);
	}, [i18n, id]);
	const handleClick = useCallback(
		(event: React.MouseEvent) => {
			event.stopPropagation();
			copyId();
		},
		[copyId],
	);
	const handleKeyDown = useCallback(
		(event: React.KeyboardEvent) => {
			event.stopPropagation();
			if (isKeyboardActivationKey(event.key)) {
				event.preventDefault();
				copyId();
			}
		},
		[copyId],
	);
	return (
		<Tooltip
			text={i18n._(CLICK_TO_COPY_ID_DESCRIPTOR)}
			data-flx="guild.guild-tabs.guild-audit-log-tab-components.copy-id-inline.tooltip"
		>
			<FocusRing offset={-2} data-flx="guild.guild-tabs.guild-audit-log-tab-components.copy-id-inline.focus-ring">
				<span
					className={clsx(styles.copyIdInline, className)}
					onClick={handleClick}
					onKeyDown={handleKeyDown}
					role="button"
					tabIndex={0}
					data-flx="guild.guild-tabs.guild-audit-log-tab-components.copy-id-inline.copy-id-inline.click"
				>
					{children}
				</span>
			</FocusRing>
		</Tooltip>
	);
};
