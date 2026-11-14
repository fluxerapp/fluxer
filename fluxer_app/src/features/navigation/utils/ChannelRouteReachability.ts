// SPDX-License-Identifier: AGPL-3.0-or-later

import {Routes} from '@app/app/Routes';
import Channels from '@app/features/channel/state/Channels';
import GuildAvailability from '@app/features/guild/state/GuildAvailability';
import Guilds from '@app/features/guild/state/Guilds';
import * as RouterUtils from '@app/features/navigation/utils/RouterUtils';
import Location from '@app/features/ui/state/Location';

function isUnreachableChannelRoute(pathname: string): boolean {
	const [, root, owner, channelId] = pathname.split('/');
	if (root !== 'channels' || owner === undefined) {
		return false;
	}
	if (owner === '@me') {
		return channelId !== undefined && channelId !== '' && Channels.getChannel(channelId) === undefined;
	}
	if (owner.startsWith('@')) {
		return false;
	}
	return Guilds.getGuild(owner) === undefined && !GuildAvailability.unavailableGuilds.has(owner);
}

export function abandonUnreachableLastLocation(): void {
	const lastLocation = Location.getLastLocation();
	if (lastLocation === null || !isUnreachableChannelRoute(lastLocation)) {
		return;
	}
	Location.clearLastLocation();
	if (RouterUtils.getCurrentPath() === lastLocation) {
		RouterUtils.replaceWith(Routes.ME);
	}
}
