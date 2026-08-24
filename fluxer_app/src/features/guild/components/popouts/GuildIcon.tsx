// SPDX-License-Identifier: AGPL-3.0-or-later

import {useHover} from '@app/features/app/hooks/useHover';
import {useShouldAnimate} from '@app/features/app/hooks/useShouldAnimate';
import styles from '@app/features/guild/components/popouts/GuildIcon.module.css';
import {getGuildIconDisplayInitials, getInitialsLength} from '@app/features/guild/utils/GuildInitialsUtils';
import * as ImageCacheUtils from '@app/features/messaging/utils/ImageCacheUtils';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import * as StringUtils from '@app/lib/strings';
import {clsx} from 'clsx';
import {observer} from 'mobx-react-lite';
import type React from 'react';
import {useEffect, useMemo, useState} from 'react';

interface GuildIconProps {
	id: string;
	name: string;
	icon: string | null;
	className?: string;
	sizePx?: number;
	containerProps?: React.HTMLAttributes<HTMLElement> & {
		'data-flx'?: string;
		'data-jump-link-guild-icon'?: string;
	};
}

type GuildIconStyleVars = React.CSSProperties & {
	'--guild-icon-size'?: string;
	'--guild-icon-image'?: string;
};

export const GuildIcon = observer(function GuildIcon({
	id,
	name,
	icon,
	className,
	sizePx,
	containerProps,
}: GuildIconProps) {
	const rawInitials = useMemo(() => StringUtils.getInitialsFromName(name), [name]);
	const initials = useMemo(() => getGuildIconDisplayInitials(rawInitials), [rawInitials]);
	const initialsLength = useMemo(() => getInitialsLength(rawInitials), [rawInitials]);
	const [hoverRef, isHovering] = useHover();
	const iconUrl = icon ? AvatarUtils.getGuildIconURL({id, icon}) : null;
	const hoverIconUrl = icon ? AvatarUtils.getGuildIconURL({id, icon}, true) : null;
	const isAnimatable = hoverIconUrl != null && hoverIconUrl !== iconUrl;
	const animationAllowed = useShouldAnimate({kind: 'guild_icon', isAnimated: isAnimatable, isHovering});
	const [loadedAnimatedUrl, setLoadedAnimatedUrl] = useState<string | null>(null);
	const isAnimatedLoaded = hoverIconUrl != null && loadedAnimatedUrl === hoverIconUrl;
	useEffect(() => {
		if (!animationAllowed || hoverIconUrl == null || isAnimatedLoaded) return;
		return ImageCacheUtils.loadImage(hoverIconUrl, () => setLoadedAnimatedUrl(hoverIconUrl));
	}, [animationAllowed, hoverIconUrl, isAnimatedLoaded]);
	const activeUrl = animationAllowed && isAnimatedLoaded ? hoverIconUrl : iconUrl;
	const paintedUrl = activeUrl != null && activeUrl.length > 0 ? activeUrl : null;
	const styleVars: GuildIconStyleVars = {};
	if (sizePx != null) {
		styleVars['--guild-icon-size'] = remFromPx(sizePx);
	}
	if (paintedUrl != null) {
		styleVars['--guild-icon-image'] = `url(${paintedUrl})`;
	}
	return (
		<div
			ref={hoverRef}
			className={clsx(styles.container, className, paintedUrl == null && styles.containerNoIcon)}
			data-flx="guild.guild-icon.container"
			{...containerProps}
			data-initials-length={initialsLength}
			style={styleVars}
		>
			{paintedUrl == null && (
				<span className={styles.initials} data-flx="guild.guild-icon.initials">
					{initials}
				</span>
			)}
		</div>
	);
});
