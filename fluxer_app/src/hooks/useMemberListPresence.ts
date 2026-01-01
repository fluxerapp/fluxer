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
import type {StatusType} from '~/Constants';
import MemberSidebarStore from '~/stores/MemberSidebarStore';
import PresenceStore from '~/stores/PresenceStore';

interface UseMemberListPresenceOptions {
	guildId: string;
	channelId: string;
	userId: string;
	enabled?: boolean;
}

export function useMemberListPresence({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListPresenceOptions): StatusType {
	const [status, setStatus] = useState(() => {
		const memberListPresence = enabled ? MemberSidebarStore.getPresence(guildId, channelId, userId) : null;
		return memberListPresence ?? PresenceStore.getStatus(userId);
	});

	useEffect(() => {
		let disposeMemberListReaction: (() => void) | undefined;

		if (enabled) {
			disposeMemberListReaction = reaction(
				() => MemberSidebarStore.getPresence(guildId, channelId, userId),
				(memberListPresence) => {
					if (memberListPresence !== null) {
						setStatus(memberListPresence);
					} else {
						setStatus(PresenceStore.getStatus(userId));
					}
				},
				{fireImmediately: true},
			);
		} else {
			setStatus(PresenceStore.getStatus(userId));
		}

		const unsubscribePresence = PresenceStore.subscribeToUserStatus(userId, (_userId, newStatus) => {
			if (!enabled) {
				setStatus(newStatus);
				return;
			}
			setStatus(() => {
				const memberListPresence = MemberSidebarStore.getPresence(guildId, channelId, userId);
				return memberListPresence ?? newStatus;
			});
		});

		return () => {
			unsubscribePresence();
			disposeMemberListReaction?.();
		};
	}, [guildId, channelId, userId, enabled]);

	return status;
}
