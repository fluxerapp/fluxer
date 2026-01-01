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

import {reaction} from 'mobx';
import {useEffect, useState} from 'react';
import type {CustomStatus} from '~/lib/customStatus';
import MemberSidebarStore from '~/stores/MemberSidebarStore';

interface UseMemberListCustomStatusOptions {
	guildId: string;
	channelId: string;
	userId: string;
	enabled?: boolean;
}

export function useMemberListCustomStatus({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListCustomStatusOptions): CustomStatus | null | undefined {
	const [customStatus, setCustomStatus] = useState<CustomStatus | null | undefined>(() => {
		if (!enabled) {
			return undefined;
		}
		return MemberSidebarStore.getCustomStatus(guildId, channelId, userId);
	});

	useEffect(() => {
		if (!enabled) {
			setCustomStatus(undefined);
			return;
		}

		const dispose = reaction(
			() => MemberSidebarStore.getCustomStatus(guildId, channelId, userId),
			(memberListCustomStatus) => {
				setCustomStatus(memberListCustomStatus);
			},
			{fireImmediately: true},
		);

		return () => {
			dispose();
		};
	}, [guildId, channelId, userId, enabled]);

	return customStatus;
}
