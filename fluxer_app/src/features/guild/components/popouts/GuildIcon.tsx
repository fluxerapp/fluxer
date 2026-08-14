// SPDX-License-Identifier: AGPL-3.0-or-later

import Accessibility from '@app/features/accessibility/state/Accessibility';
import {useHover} from '@app/features/app/hooks/useHover';
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

function resolveInitiallyLoadedImageUrl(url: string | null): string | null {
	if (url == null) return null;
	if (ImageCacheUtils.hasImage(url)) return url;
	return null;
}

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
	const iconUrl = useMemo(() => (icon ? AvatarUtils.getGuildIconURL({id, icon}) : null), [id, icon]);
	const hoverIconUrl = useMemo(() => (icon ? AvatarUtils.getGuildIconURL({id, icon}, true) : null), [id, icon]);
	const [loadedStaticUrl, setLoadedStaticUrl] = useState<string | null>(() => resolveInitiallyLoadedImageUrl(iconUrl));
	const [loadedAnimatedUrl, setLoadedAnimatedUrl] = useState<string | null>(() =>
		resolveInitiallyLoadedImageUrl(hoverIconUrl),
	);
	const isStaticLoaded = iconUrl != null && loadedStaticUrl === iconUrl;
	const isAnimatedLoaded = hoverIconUrl != null && loadedAnimatedUrl === hoverIconUrl;
	useEffect(() => {
		if (iconUrl == null || iconUrl.length === 0 || isStaticLoaded) return;
		let active = true;
		const cleanup = ImageCacheUtils.loadImage(iconUrl, () => {
			if (active) setLoadedStaticUrl(iconUrl);
		});
		return () => {
			active = false;
			cleanup();
		};
	}, [iconUrl, isStaticLoaded]);
	useEffect(() => {
		if (!isHovering || hoverIconUrl == null || hoverIconUrl.length === 0 || isAnimatedLoaded) return;
		let active = true;
		const cleanup = ImageCacheUtils.loadImage(hoverIconUrl, () => {
			if (active) setLoadedAnimatedUrl(hoverIconUrl);
		});
		return () => {
			active = false;
			cleanup();
		};
	}, [isHovering, hoverIconUrl, isAnimatedLoaded]);
	const shouldPlayAnimated = isHovering && isAnimatedLoaded;
	const activeUrl = shouldPlayAnimated && hoverIconUrl != null ? hoverIconUrl : iconUrl;
	const styleVars: GuildIconStyleVars = {};
	if (sizePx != null) {
		styleVars['--guild-icon-size'] = remFromPx(sizePx);
	}
	if (isStaticLoaded && activeUrl) {
		styleVars['--guild-icon-image'] = `url(${activeUrl})`;
	}
	const reducedMotion = Accessibility.useReducedMotion;
	return (
		<div
			ref={hoverRef}
			className={clsx(styles.container, className, !icon && styles.containerNoIcon)}
			data-flx="guild.guild-icon.container"
			{...containerProps}
			data-initials-length={initialsLength}
			data-reduced-motion={reducedMotion}
			style={styleVars}
		>
			{!icon && (
				<span className={styles.initials} data-flx="guild.guild-icon.initials">
					{initials}
				</span>
			)}
		</div>
	);
});
