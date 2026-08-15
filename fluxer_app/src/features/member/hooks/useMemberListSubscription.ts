// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewayConnection from '@app/features/gateway/transport/GatewayConnection';
import MemberSidebar from '@app/features/member/state/MemberSidebar';
import {
	areNormalizedMemberListRangesEqual,
	type MemberListRanges,
	type NormalizedMemberListRanges,
	normalizeMemberListRanges,
} from '@app/features/member/utils/MemberListRangeUtils';
import {Logger} from '@app/features/platform/utils/AppLogger';
import {MEMBER_LIST_RANGE_MAX_SPAN} from '@fluxer/constants/src/GatewayConstants';
import {reaction} from 'mobx';
import {useCallback, useEffect, useRef, useState} from 'react';

interface UseMemberListSubscriptionOptions {
	guildId: string;
	channelId: string;
	enabled: boolean;
}

interface UseMemberListSubscriptionResult {
	subscribe: (ranges: MemberListRanges) => void;
}

interface MemberListSubscriptionControl {
	handleDesiredRanges: (changed: boolean) => void;
	verifyHydration: () => void;
}

type MemberListSubscriptionPhase = 'offline' | 'hydrating' | 'settled' | 'freshSession';

const INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES = normalizeMemberListRanges([[0, MEMBER_LIST_RANGE_MAX_SPAN]]);
const MEMBER_LIST_RECOVERY_DELAY_MS = 3000;
const MEMBER_LIST_MAX_RECOVERY_ATTEMPTS = 2;
const logger = new Logger('useMemberListSubscription');

let nextMemberListSubscriptionOwnerId = 0;

function createMemberListSubscriptionOwnerId(): string {
	nextMemberListSubscriptionOwnerId += 1;
	return `member-list-subscription:${nextMemberListSubscriptionOwnerId}`;
}

function resolveDesiredRanges(ranges: MemberListRanges): NormalizedMemberListRanges {
	const normalizedRanges = normalizeMemberListRanges(ranges);
	return normalizedRanges.length > 0 ? normalizedRanges : INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES;
}

export function useMemberListSubscription({
	guildId,
	channelId,
	enabled,
}: UseMemberListSubscriptionOptions): UseMemberListSubscriptionResult {
	const [ownerId] = useState(createMemberListSubscriptionOwnerId);
	const desiredRangesRef = useRef<NormalizedMemberListRanges>(INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES);
	const controlRef = useRef<MemberListSubscriptionControl | null>(null);

	const subscribe = useCallback(
		(ranges: MemberListRanges) => {
			if (!enabled) {
				return;
			}
			const nextDesiredRanges = resolveDesiredRanges(ranges);
			const desiredRangesChanged = !areNormalizedMemberListRangesEqual(desiredRangesRef.current, nextDesiredRanges);
			desiredRangesRef.current = nextDesiredRanges;
			const control = controlRef.current;
			if (control != null) {
				control.handleDesiredRanges(desiredRangesChanged);
				if (!desiredRangesChanged) {
					control.verifyHydration();
				}
				return;
			}
			if (!MemberSidebar.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			MemberSidebar.subscribeToChannel(guildId, channelId, desiredRangesRef.current, ownerId);
		},
		[guildId, channelId, enabled, ownerId],
	);

	useEffect(() => {
		desiredRangesRef.current = INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES;
	}, [guildId, channelId]);

	useEffect(() => {
		if (!enabled) {
			return;
		}
		let disposed = false;
		let reconciliationScheduled = false;
		let retryTimer: number | null = null;
		let hydrationGeneration = 0;
		let freshSessionResetId: string | null = null;
		let recoveryAttemptCount = 0;
		let phase: MemberListSubscriptionPhase = 'offline';

		function clearRetryTimer(): void {
			if (retryTimer == null) {
				return;
			}
			window.clearTimeout(retryTimer);
			retryTimer = null;
		}

		function gatewayAvailable(): boolean {
			return GatewayConnection.isReady && GatewayConnection.isConnected && GatewayConnection.sessionId != null;
		}

		function ownsSubscription(): boolean {
			return MemberSidebar.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId);
		}

		function hasHydratedDesiredRanges(): boolean {
			return MemberSidebar.hasHydratedRanges(guildId, channelId, desiredRangesRef.current);
		}

		function beginRecoveryDemand(): void {
			clearRetryTimer();
			hydrationGeneration += 1;
			recoveryAttemptCount = 0;
		}

		function replaceStaleSession(): void {
			clearRetryTimer();
			if (
				disposed ||
				recoveryAttemptCount >= MEMBER_LIST_MAX_RECOVERY_ATTEMPTS ||
				phase === 'freshSession' ||
				!ownsSubscription()
			) {
				return;
			}
			const socket = GatewayConnection.socket;
			if (!gatewayAvailable() || socket == null) {
				phase = 'offline';
				return;
			}
			freshSessionResetId = GatewayConnection.sessionId;
			if (freshSessionResetId == null) {
				phase = 'offline';
				return;
			}
			recoveryAttemptCount += 1;
			phase = 'freshSession';
			logger.warn('Member list hydration stalled; replacing the stale Gateway session', {guildId, channelId});
			socket.reset(true);
		}

		function scheduleSessionReplacement(): void {
			if (recoveryAttemptCount >= MEMBER_LIST_MAX_RECOVERY_ATTEMPTS || retryTimer != null) {
				return;
			}
			const scheduledGeneration = hydrationGeneration;
			retryTimer = window.setTimeout(() => {
				retryTimer = null;
				if (scheduledGeneration !== hydrationGeneration) {
					return;
				}
				replaceStaleSession();
			}, MEMBER_LIST_RECOVERY_DELAY_MS);
		}

		function verifyHydration(): void {
			if (disposed || !ownsSubscription()) {
				return;
			}
			if (phase === 'freshSession') {
				return;
			}
			if (hasHydratedDesiredRanges()) {
				clearRetryTimer();
				recoveryAttemptCount = 0;
				freshSessionResetId = null;
				phase = 'settled';
				return;
			}
			if (phase === 'settled' || phase === 'hydrating') {
				phase = 'hydrating';
				scheduleSessionReplacement();
			}
		}

		function sendDesiredRequest(): void {
			clearRetryTimer();
			if (disposed || !gatewayAvailable()) {
				phase = 'offline';
				return;
			}
			if (!ownsSubscription()) {
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			if (!MemberSidebar.retryChannelSubscription(guildId, channelId, desiredRangesRef.current, ownerId)) {
				phase = 'offline';
				return;
			}
			phase = 'hydrating';
			verifyHydration();
		}

		function handleDesiredRanges(changed: boolean): void {
			if (disposed) {
				return;
			}
			const wasOwner = ownsSubscription();
			if (!wasOwner) {
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			const reclaimedOwnership = !wasOwner && ownsSubscription();
			if (changed || reclaimedOwnership) {
				beginRecoveryDemand();
			}
			if (!gatewayAvailable()) {
				phase = 'offline';
				return;
			}
			if (!ownsSubscription()) {
				return;
			}
			MemberSidebar.updateChannelSubscriptionRangesLocally(guildId, channelId, desiredRangesRef.current, ownerId);
			if (!changed && !reclaimedOwnership) {
				return;
			}
			if (phase === 'freshSession') {
				return;
			}
			sendDesiredRequest();
		}

		function reconcileSubscription(): void {
			if (disposed || !gatewayAvailable()) {
				return;
			}
			if (
				phase === 'freshSession' &&
				freshSessionResetId != null &&
				GatewayConnection.sessionId === freshSessionResetId
			) {
				return;
			}
			freshSessionResetId = null;
			if (!ownsSubscription()) {
				if (MemberSidebar.hasActiveMemberListSubscription()) {
					return;
				}
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			sendDesiredRequest();
		}

		function scheduleReconciliation(): void {
			if (disposed || reconciliationScheduled) {
				return;
			}
			reconciliationScheduled = true;
			queueMicrotask(() => {
				try {
					reconcileSubscription();
				} finally {
					reconciliationScheduled = false;
				}
			});
		}

		const control: MemberListSubscriptionControl = {handleDesiredRanges, verifyHydration};
		controlRef.current = control;
		const disposeSessionReaction = reaction(
			() => MemberSidebar.sessionVersion,
			() => scheduleReconciliation(),
		);
		const disposeSubscriptionGenerationReaction = reaction(
			() => MemberSidebar.memberListSubscriptionGeneration,
			() => scheduleReconciliation(),
		);
		const disposeGatewayAvailabilityReaction = reaction(
			() => GatewayConnection.isReady && GatewayConnection.isConnected,
			(isAvailable) => {
				if (!isAvailable) {
					clearRetryTimer();
					MemberSidebar.handleGatewayDisconnected();
					if (phase !== 'freshSession') {
						phase = 'offline';
					}
					return;
				}
				scheduleReconciliation();
			},
		);
		const disposeListPresenceReaction = reaction(
			() => MemberSidebar.getList(guildId, channelId) != null,
			(hasList) => {
				if (!hasList) {
					scheduleReconciliation();
				}
			},
		);
		const disposeHydrationReaction = reaction(
			() => MemberSidebar.hasHydratedRanges(guildId, channelId, desiredRangesRef.current),
			() => verifyHydration(),
			{fireImmediately: true},
		);
		scheduleReconciliation();

		return () => {
			disposed = true;
			if (controlRef.current === control) {
				controlRef.current = null;
			}
			clearRetryTimer();
			disposeSessionReaction();
			disposeSubscriptionGenerationReaction();
			disposeGatewayAvailabilityReaction();
			disposeListPresenceReaction();
			disposeHydrationReaction();
			MemberSidebar.releaseMemberListSubscription(guildId, channelId, ownerId);
		};
	}, [guildId, channelId, enabled, ownerId]);

	return {subscribe};
}
