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

import {useMemo} from 'react';
import {GuildFeatures} from '~/Constants';
import type {GuildRecord} from '~/records/GuildRecord';
import * as AvatarUtils from '~/utils/AvatarUtils';
import styles from './GuildDetachedBanner.module.css';

const MAX_VIEWPORT_HEIGHT_FRACTION = 0.3;
const DEFAULT_BANNER_HEIGHT = 240;

export function GuildDetachedBanner({guild}: {guild: GuildRecord}) {
	const aspectRatio = useMemo(
		() => (guild.bannerWidth && guild.bannerHeight ? guild.bannerWidth / guild.bannerHeight : undefined),
		[guild.bannerHeight, guild.bannerWidth],
	);
	const bannerURL = AvatarUtils.getGuildBannerURL({id: guild.id, banner: guild.banner}, true);
	const isDetachedBanner = guild.features.has(GuildFeatures.DETACHED_BANNER);

	if (!bannerURL || !isDetachedBanner) return null;

	const maxHeight = `${MAX_VIEWPORT_HEIGHT_FRACTION * 100}vh`;
	const bannerHeight = guild.bannerHeight ?? DEFAULT_BANNER_HEIGHT;

	return (
		<div
			className={styles.container}
			style={{maxHeight, ...(aspectRatio ? {aspectRatio: `${aspectRatio}`} : {height: bannerHeight})}}
		>
			<img src={bannerURL} alt="" className={styles.banner} draggable={false} />
		</div>
	);
}
