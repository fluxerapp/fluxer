// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/app/components/layout/GuildDetachedBanner.module.css';
import {useAnimatedImageUrl} from '@app/features/app/hooks/useAnimatedImageUrl';
import {clampWideAssetAspectRatio} from '@app/features/expressions/utils/AssetImageGeometry';
import type {Guild} from '@app/features/guild/models/Guild';
import {remFromPx} from '@app/features/theme/layout/RemFromPx';
import * as AvatarUtils from '@app/features/user/utils/AvatarUtils';
import {GuildFeatures} from '@fluxer/constants/src/GuildConstants';
import {observer} from 'mobx-react-lite';
import {useMemo} from 'react';

const MAX_VIEWPORT_HEIGHT_FRACTION = 0.3;
const DEFAULT_BANNER_HEIGHT = 240;
export const GuildDetachedBanner = observer(function GuildDetachedBanner({guild}: {guild: Guild}) {
	const aspectRatio = useMemo(
		() =>
			guild.bannerWidth && guild.bannerHeight
				? clampWideAssetAspectRatio(guild.bannerWidth / guild.bannerHeight)
				: undefined,
		[guild.bannerHeight, guild.bannerWidth],
	);
	const isDetachedBanner = guild.features.has(GuildFeatures.DETACHED_BANNER);
	const staticBannerURL = useMemo(
		() =>
			isDetachedBanner ? AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, false) || null : null,
		[guild.banner, guild.id, isDetachedBanner],
	);
	const animatedBannerURL = useMemo(
		() => (isDetachedBanner ? AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true) || null : null),
		[guild.banner, guild.id, isDetachedBanner],
	);
	const {hoverRef: bannerHoverRef, imageUrl: bannerURL} = useAnimatedImageUrl({
		staticUrl: staticBannerURL,
		animatedUrl: animatedBannerURL,
		kind: 'gif',
	});
	if (!bannerURL) return null;
	const maxHeight = `${MAX_VIEWPORT_HEIGHT_FRACTION * 100}vh`;
	const bannerHeight = guild.bannerHeight ?? DEFAULT_BANNER_HEIGHT;
	return (
		<div
			ref={bannerHoverRef}
			className={styles.container}
			style={{maxHeight, ...(aspectRatio ? {aspectRatio: `${aspectRatio}`} : {height: remFromPx(bannerHeight)})}}
			data-flx="app.guild-detached-banner.container"
		>
			<img
				src={bannerURL}
				alt=""
				className={styles.banner}
				draggable={false}
				data-flx="app.guild-detached-banner.banner"
			/>
		</div>
	);
});
