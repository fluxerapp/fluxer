// SPDX-License-Identifier: AGPL-3.0-or-later

import MemberSidebar from '@app/features/member/state/MemberSidebar';
import Presence from '@app/features/presence/state/Presence';
import TransientPresence from '@app/features/presence/state/TransientPresence';
import type {StatusType} from '@fluxer/constants/src/StatusConstants';
import {isOfflineStatus, StatusTypes} from '@fluxer/constants/src/StatusConstants';
import {reaction} from 'mobx';
import {useCallback, useEffect, useState} from 'react';

interface UseMemberListPresenceOptions {
	guildId: string;
	channelId: string;
	userId: string;
	enabled?: boolean;
}

export function resolveMemberListPresence({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListPresenceOptions): StatusType {
	const memberListPresence = enabled ? MemberSidebar.getPresence(guildId, channelId, userId) : null;
	if (memberListPresence !== null) {
		return memberListPresence;
	}
	const presenceStatus = Presence.getStatus(userId);
	if (!isOfflineStatus(presenceStatus)) {
		return presenceStatus;
	}
	const transientStatus = TransientPresence.getTransientStatus(userId);
	if (transientStatus !== null && !isOfflineStatus(transientStatus)) {
		return transientStatus;
	}
	return transientStatus ?? StatusTypes.OFFLINE;
}

export function useMemberListPresence({
	guildId,
	channelId,
	userId,
	enabled = true,
}: UseMemberListPresenceOptions): StatusType {
	const hasMemberListSource = guildId !== '';
	const computeStatus = useCallback(
		() =>
			resolveMemberListPresence({
				guildId,
				channelId,
				userId,
				enabled: hasMemberListSource,
			}),
		[channelId, guildId, hasMemberListSource, userId],
	);
	const [status, setStatus] = useState<StatusType>(() => (enabled ? computeStatus() : StatusTypes.OFFLINE));
	useEffect(() => {
		if (!enabled) {
			return;
		}
		let lastStatus = computeStatus();
		setStatus(lastStatus);
		const applyStatus = () => {
			const nextStatus = computeStatus();
			if (nextStatus === lastStatus) {
				return;
			}
			lastStatus = nextStatus;
			setStatus(nextStatus);
		};
		const disposeMemberListReaction = hasMemberListSource
			? reaction(() => MemberSidebar.getPresence(guildId, channelId, userId), applyStatus)
			: undefined;
		const unsubscribePresence = Presence.subscribeToUserStatus(userId, applyStatus);
		const disposeTransient = reaction(() => TransientPresence.getTransientStatus(userId), applyStatus);
		return () => {
			unsubscribePresence();
			disposeTransient();
			disposeMemberListReaction?.();
		};
	}, [computeStatus, enabled, hasMemberListSource, guildId, channelId, userId]);
	return status;
}
