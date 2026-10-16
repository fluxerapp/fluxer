// SPDX-License-Identifier: AGPL-3.0-or-later

import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {filterExpiredStatus, useCustomStatusExpiryTick} from '@app/features/presence/hooks/usePresenceCustomStatus';
import Presence from '@app/features/presence/state/Presence';
import type {CustomStatus} from '@app/features/user/state/CustomStatus';
import {CustomStatusEmitter} from '@app/features/user/state/CustomStatusEmitter';
import {isOfflineStatus} from '@fluxer/constants/src/StatusConstants';
import {useCallback, useSyncExternalStore} from 'react';

interface UseMemberListCustomStatusOptions {
	guildId: string;
	channelId: string;
	userId: string;
	enabled?: boolean;
}

export function resolveMemberListCustomStatus({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListCustomStatusOptions): CustomStatus | null {
	const memberListPresence = enabled ? MemberSidebar.getPresence(guildId, channelId, userId) : null;
	if (memberListPresence !== null && isOfflineStatus(memberListPresence)) {
		return null;
	}
	const memberListStatus = enabled ? MemberSidebar.getCustomStatus(guildId, channelId, userId) : undefined;
	if (memberListStatus !== undefined) {
		return filterExpiredStatus(memberListStatus);
	}
	if (isOfflineStatus(Presence.getStatus(userId))) {
		return null;
	}
	return filterExpiredStatus(Presence.getCustomStatus(userId));
}

export function useMemberListCustomStatus({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListCustomStatusOptions): CustomStatus | null | undefined {
	const expiryTick = useCustomStatusExpiryTick(
		enabled ? MemberSidebar.getCustomStatus(guildId, channelId, userId)?.expiresAt : undefined,
	);
	const subscribe = useCallback(
		(onChange: () => void) => {
			if (!enabled) {
				return () => {};
			}
			return CustomStatusEmitter.subscribeToMemberList(guildId, channelId, userId, onChange);
		},
		[guildId, channelId, userId, enabled],
	);
	const getSnapshot = useCallback((): CustomStatus | null | undefined => {
		if (!enabled) {
			return undefined;
		}
		const memberListPresence = MemberSidebar.getPresence(guildId, channelId, userId);
		if (memberListPresence !== null && isOfflineStatus(memberListPresence)) {
			return null;
		}
		return filterExpiredStatus(MemberSidebar.getCustomStatus(guildId, channelId, userId));
	}, [guildId, channelId, userId, enabled, expiryTick]);
	const customStatus = useSyncExternalStore(subscribe, getSnapshot, getSnapshot);

	return customStatus;
}
