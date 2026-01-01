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

import {useEffect, useState} from 'react';
import {Permissions} from '~/Constants';
import type {ChannelRecord} from '~/records/ChannelRecord';
import DeveloperOptionsStore from '~/stores/DeveloperOptionsStore';
import PermissionStore from '~/stores/PermissionStore';
import SlowmodeStore from '~/stores/SlowmodeStore';
import UserStore from '~/stores/UserStore';

interface SlowmodeState {
	isSlowmodeActive: boolean;
	slowmodeRemaining: number;
	canBypass: boolean;
}

export function useSlowmode(channel: ChannelRecord): SlowmodeState {
	const [slowmodeRemaining, setSlowmodeRemaining] = useState(0);
	const currentUser = UserStore.getCurrentUser();
	const lastSendTimestamp = SlowmodeStore.getLastSendTimestamp(channel.id);
	const mockSlowmodeActive = DeveloperOptionsStore.mockSlowmodeActive;
	const mockSlowmodeRemaining = DeveloperOptionsStore.mockSlowmodeRemaining;

	const canBypass = channel.guildId ? PermissionStore.can(Permissions.BYPASS_SLOWMODE, channel) : true;

	const rateLimitPerUser = channel.rateLimitPerUser || 0;

	useEffect(() => {
		if (mockSlowmodeActive) {
			setSlowmodeRemaining(mockSlowmodeRemaining);
			return;
		}

		if (!currentUser || !channel.guildId || !rateLimitPerUser || canBypass) {
			setSlowmodeRemaining(0);
			return;
		}

		const updateSlowmode = () => {
			if (!lastSendTimestamp) {
				setSlowmodeRemaining(0);
				return;
			}

			const timeSinceLastMessage = Date.now() - lastSendTimestamp;
			const remaining = Math.max(0, rateLimitPerUser * 1000 - timeSinceLastMessage);

			setSlowmodeRemaining(remaining);
		};

		updateSlowmode();
	}, [
		channel.guildId,
		currentUser,
		rateLimitPerUser,
		canBypass,
		lastSendTimestamp,
		mockSlowmodeActive,
		mockSlowmodeRemaining,
	]);

	useEffect(() => {
		if (mockSlowmodeActive) return;
		if (slowmodeRemaining <= 0) return;

		const interval = setInterval(() => {
			setSlowmodeRemaining((prev) => {
				const next = prev - 1000;
				return next > 0 ? next : 0;
			});
		}, 1000);

		return () => clearInterval(interval);
	}, [slowmodeRemaining, mockSlowmodeActive]);

	const isSlowmodeActive = mockSlowmodeActive || (!canBypass && rateLimitPerUser > 0 && slowmodeRemaining > 0);

	return {
		isSlowmodeActive,
		slowmodeRemaining,
		canBypass,
	};
}
