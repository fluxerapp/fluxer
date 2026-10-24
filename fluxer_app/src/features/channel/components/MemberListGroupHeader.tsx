// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/channel/components/ChannelMembers.module.css';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {getCachedNumberFormat} from '@app/features/i18n/utils/IntlCache';
import {openRoleContextMenu, openRoleContextMenuForElement} from '@app/features/ui/action_menu/RoleContextMenu';
import {useLingui} from '@lingui/react/macro';
import type {CSSProperties, Ref} from 'react';

interface MemberListGroupHeaderProps {
	groupId: string;
	guildRole: GuildRole | null;
	name: string;
	count: number;
	className: string;
	style?: CSSProperties;
	ref?: Ref<HTMLDivElement>;
}

export function MemberListGroupHeader({
	groupId,
	guildRole,
	name,
	count,
	className,
	style,
	ref,
}: MemberListGroupHeaderProps) {
	const {i18n} = useLingui();
	return (
		// biome-ignore lint/a11y/noStaticElementInteractions: Role headings have button semantics and handlers; other headings have neither.
		<div
			ref={ref}
			data-member-group-id={groupId}
			className={className}
			style={style}
			role={guildRole ? 'button' : undefined}
			tabIndex={guildRole ? 0 : undefined}
			onContextMenu={guildRole ? (event) => openRoleContextMenu(event, guildRole.id) : undefined}
			onKeyDown={
				guildRole
					? (event) => {
							if (event.key !== 'ContextMenu' && !(event.shiftKey && event.key === 'F10')) return;
							event.preventDefault();
							event.stopPropagation();
							openRoleContextMenuForElement(event.currentTarget, guildRole.id);
						}
					: undefined
			}
			data-member-list-focus-item={guildRole ? 'true' : undefined}
			data-flx="channel.member-list-group-header"
		>
			<span className={styles.groupHeaderLabel} data-flx="channel.member-list-group-header.label">
				{name}
			</span>
			<span className={styles.groupHeaderSeparator} data-flx="channel.member-list-group-header.separator">
				{'—'}
			</span>
			<span className={styles.groupHeaderCount} data-flx="channel.member-list-group-header.count">
				{getCachedNumberFormat(i18n.locale).format(count)}
			</span>
		</div>
	);
}
