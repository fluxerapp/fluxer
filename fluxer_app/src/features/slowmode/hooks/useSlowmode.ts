// SPDX-License-Identifier: AGPL-3.0-or-later

import type {Channel} from '@app/features/channel/models/Channel';
import DeveloperOptions from '@app/features/devtools/state/DeveloperOptions';
import Permission from '@app/features/permissions/state/Permission';
import Slowmode from '@app/features/slowmode/state/Slowmode';
import {useNow} from '@app/features/ui/state/Tick';
import Users from '@app/features/user/state/Users';
import {Permissions} from '@fluxer/constants/src/ChannelConstants';
import {CHANNEL_RATE_LIMIT_PER_USER_MAX} from '@fluxer/constants/src/LimitConstants';

interface SlowmodeState {
	isSlowmodeActive: boolean;
	slowmodeRemaining: number;
	canBypass: boolean;
	isSlowmodeEnabled: boolean;
	isSlowmodeImmune: boolean;
}

export function useSlowmode(channel: Channel): SlowmodeState {
	const currentUser = Users.getCurrentUser();
	const mockSlowmodeActive = DeveloperOptions.mockSlowmodeActive;
	const mockSlowmodeRemaining = DeveloperOptions.mockSlowmodeRemaining;
	let canBypass = true;
	if (channel.guildId) {
		canBypass = Permission.can(Permissions.BYPASS_SLOWMODE, channel);
	}
	let rateLimitPerUser = channel.rateLimitPerUser;
	if (rateLimitPerUser == null) {
		rateLimitPerUser = 0;
	}
	const hasValidSlowmodeRateLimit =
		Number.isSafeInteger(rateLimitPerUser) &&
		rateLimitPerUser > 0 &&
		rateLimitPerUser <= CHANNEL_RATE_LIMIT_PER_USER_MAX;
	let slowmodeRemaining = 0;
	if (mockSlowmodeActive) {
		slowmodeRemaining = mockSlowmodeRemaining;
	} else if (currentUser && channel.guildId && hasValidSlowmodeRateLimit && !canBypass) {
		slowmodeRemaining = Slowmode.getSlowmodeRemaining(channel.id, rateLimitPerUser);
	}
	const isCountingDown = !mockSlowmodeActive && slowmodeRemaining > 0;
	useNow(isCountingDown);
	const isSlowmodeEnabled = mockSlowmodeActive || (Boolean(channel.guildId) && hasValidSlowmodeRateLimit);
	const isSlowmodeImmune = !mockSlowmodeActive && isSlowmodeEnabled && canBypass;
	const isSlowmodeActive = mockSlowmodeActive || (!canBypass && hasValidSlowmodeRateLimit && slowmodeRemaining > 0);
	return {
		isSlowmodeActive,
		slowmodeRemaining,
		canBypass,
		isSlowmodeEnabled,
		isSlowmodeImmune,
	};
}
