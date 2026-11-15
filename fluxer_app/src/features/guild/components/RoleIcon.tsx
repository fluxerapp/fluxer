// SPDX-License-Identifier: AGPL-3.0-or-later

import * as EmojiUtils from '@app/features/expressions/utils/EmojiUtils';
import UnicodeEmojis from '@app/features/expressions/utils/UnicodeEmojis';
import styles from '@app/features/guild/components/RoleIcon.module.css';
import type {GuildRole} from '@app/features/guild/models/GuildRole';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import {Tooltip} from '@app/features/ui/tooltip/Tooltip';
import {getRoleIconURL} from '@app/features/user/utils/AvatarUtils';
import {msg} from '@lingui/core/macro';
import {useLingui} from '@lingui/react/macro';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import {useState} from 'react';

export type RoleIconSize = 16 | 20 | 40;

const ROLE_ICON_LABEL_DESCRIPTOR = msg({
	message: 'Role icon, {roleName}',
	comment:
		'Accessible label read by screen readers for the small icon next to a member name. roleName is the name of the community role the icon belongs to.',
});

const EMOJI_IMAGE_SIZE: Record<RoleIconSize, number> = {16: 14, 20: 18, 40: 36};

interface RoleIconProps {
	role: Pick<GuildRole, 'id' | 'name' | 'icon' | 'unicodeEmoji'>;
	size: RoleIconSize;
	decorative?: boolean;
	className?: string;
}

function getImageSize(role: RoleIconProps['role'], size: RoleIconSize): number {
	return role.icon ? size : EMOJI_IMAGE_SIZE[size];
}

function resolveRoleIconSource(role: RoleIconProps['role'], size: RoleIconSize): string | null {
	if (role.icon) {
		return getRoleIconURL({id: role.id, icon: role.icon}, size) || null;
	}
	if (role.unicodeEmoji) {
		return EmojiUtils.getEmojiURL(UnicodeEmojis.normalizeEmojiNameToSurrogate(role.unicodeEmoji));
	}
	return null;
}

export const RoleIcon = observer(function RoleIcon({role, size, decorative = false, className}: RoleIconProps) {
	const {i18n} = useLingui();
	const src = resolveRoleIconSource(role, size);
	const [failedSrc, setFailedSrc] = useState<string | null>(null);
	if (!src || failedSrc === src) {
		return null;
	}
	const imageSize = getImageSize(role, size);
	const labelProps = decorative
		? {'aria-hidden': true}
		: {role: 'img', 'aria-label': i18n._(ROLE_ICON_LABEL_DESCRIPTOR, {roleName: role.name})};
	const icon = (
		<span
			className={clsx(styles.roleIcon, className)}
			style={{width: remFromPx(size), height: remFromPx(size)}}
			{...labelProps}
			data-flx="guild.role-icon.role-icon"
		>
			<img
				src={src}
				alt=""
				style={{width: remFromPx(imageSize), height: remFromPx(imageSize)}}
				draggable={false}
				loading="lazy"
				className={styles.roleIconImage}
				onError={() => setFailedSrc(src)}
				data-flx="guild.role-icon.role-icon-image"
			/>
		</span>
	);
	if (decorative) {
		return icon;
	}
	return (
		<Tooltip text={role.name} ariaHidden={true} data-flx="guild.role-icon.tooltip">
			{icon}
		</Tooltip>
	);
});
