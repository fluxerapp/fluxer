// SPDX-License-Identifier: AGPL-3.0-or-later

import {BadgeListModal} from '@app/features/user/components/modals/BadgeListModal';
import {customBadgeImageUrl} from '@app/features/user/utils/ProfileUtils';
import type {InstanceGuildBadge} from '@fluxer/instance_bootstrap/src/Types';

export function GuildBadgesModal({
	badges,
	'data-flx': dataFlx = 'guild.guild-badges-modal',
}: {
	readonly badges: ReadonlyArray<InstanceGuildBadge>;
	readonly 'data-flx'?: string;
}) {
	return (
		<BadgeListModal
			items={badges.map((badge) => ({
				key: badge.id,
				imageUrl: customBadgeImageUrl(badge.asset_url),
				caption: null,
				title: badge.name,
				lines: badge.description === null ? [] : [badge.description],
				href: badge.href,
			}))}
			data-flx={dataFlx}
		/>
	);
}
