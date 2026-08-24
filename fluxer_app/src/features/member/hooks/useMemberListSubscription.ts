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

type MemberListSubscriptionPhase = 'offline' | 'hydrating' | 'settled';

const INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES = normalizeMemberListRanges([[0, MEMBER_LIST_RANGE_MAX_SPAN]]);
const MEMBER_LIST_SUBSCRIPTION_SETTLE_MS = 50;
const MEMBER_LIST_RESUBSCRIBE_DELAY_MS = 1000;
const MEMBER_LIST_MAX_RESUBSCRIBE_ATTEMPTS = 3;
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
	const pendingDesiredRangesRef = useRef<NormalizedMemberListRanges | null>(null);
	const settleTimerRef = useRef<number | null>(null);
	const controlRef = useRef<MemberListSubscriptionControl | null>(null);

	const clearSettleTimer = useCallback(() => {
		if (settleTimerRef.current == null) {
			return;
		}
		window.clearTimeout(settleTimerRef.current);
		settleTimerRef.current = null;
	}, []);

	const sendWithoutControl = useCallback(
		(nextDesiredRanges: NormalizedMemberListRanges) => {
			if (!MemberSidebar.isActiveMemberListSubscriptionOwner(guildId, channelId, ownerId)) {
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			MemberSidebar.subscribeToChannel(guildId, channelId, nextDesiredRanges, ownerId);
		},
		[guildId, channelId, ownerId],
	);

	const commitDesiredRanges = useCallback(() => {
		settleTimerRef.current = null;
		const nextDesiredRanges = pendingDesiredRangesRef.current;
		pendingDesiredRangesRef.current = null;
		if (nextDesiredRanges == null) {
			return;
		}
		desiredRangesRef.current = nextDesiredRanges;
		const control = controlRef.current;
		if (control != null) {
			control.handleDesiredRanges(true);
			return;
		}
		sendWithoutControl(nextDesiredRanges);
	}, [sendWithoutControl]);

	const subscribe = useCallback(
		(ranges: MemberListRanges) => {
			if (!enabled) {
				return;
			}
			const nextDesiredRanges = resolveDesiredRanges(ranges);
			if (areNormalizedMemberListRangesEqual(desiredRangesRef.current, nextDesiredRanges)) {
				pendingDesiredRangesRef.current = null;
				clearSettleTimer();
				const control = controlRef.current;
				if (control != null) {
					control.handleDesiredRanges(false);
					control.verifyHydration();
					return;
				}
				sendWithoutControl(desiredRangesRef.current);
				return;
			}
			pendingDesiredRangesRef.current = nextDesiredRanges;
			clearSettleTimer();
			settleTimerRef.current = window.setTimeout(commitDesiredRanges, MEMBER_LIST_SUBSCRIPTION_SETTLE_MS);
		},
		[enabled, clearSettleTimer, commitDesiredRanges, sendWithoutControl],
	);

	useEffect(() => {
		desiredRangesRef.current = INITIAL_MEMBER_LIST_SUBSCRIPTION_RANGES;
		pendingDesiredRangesRef.current = null;
		clearSettleTimer();
	}, [guildId, channelId, enabled, clearSettleTimer]);

	useEffect(() => {
		return () => {
			pendingDesiredRangesRef.current = null;
			clearSettleTimer();
		};
	}, [clearSettleTimer]);

	useEffect(() => {
		if (!enabled) {
			return;
		}
		let disposed = false;
		let reconciliationScheduled = false;
		let retryTimer: number | null = null;
		let hydrationGeneration = 0;
		let resubscribeAttemptCount = 0;
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
			resubscribeAttemptCount = 0;
		}

		function resendStaleSubscription(): void {
			clearRetryTimer();
			if (disposed || !ownsSubscription()) {
				return;
			}
			if (!gatewayAvailable()) {
				phase = 'offline';
				return;
			}
			resubscribeAttemptCount += 1;
			logger.debug('Member list hydration stalled; resending the subscription', {
				guildId,
				channelId,
				attempt: resubscribeAttemptCount,
			});
			sendDesiredRequest(true);
		}

		function scheduleResubscribe(): void {
			if (retryTimer != null || resubscribeAttemptCount >= MEMBER_LIST_MAX_RESUBSCRIBE_ATTEMPTS) {
				return;
			}
			const scheduledGeneration = hydrationGeneration;
			retryTimer = window.setTimeout(() => {
				retryTimer = null;
				if (scheduledGeneration !== hydrationGeneration) {
					return;
				}
				resendStaleSubscription();
			}, MEMBER_LIST_RESUBSCRIBE_DELAY_MS);
		}

		function verifyHydration(): void {
			if (disposed || !ownsSubscription()) {
				return;
			}
			if (hasHydratedDesiredRanges()) {
				clearRetryTimer();
				resubscribeAttemptCount = 0;
				phase = 'settled';
				return;
			}
			if (phase === 'settled' || phase === 'hydrating') {
				phase = 'hydrating';
				scheduleResubscribe();
			}
		}

		function sendDesiredRequest(forceResend = false): void {
			clearRetryTimer();
			if (disposed || !gatewayAvailable()) {
				phase = 'offline';
				return;
			}
			if (!ownsSubscription()) {
				MemberSidebar.claimMemberListSubscription(guildId, channelId, ownerId);
			}
			const reachedWire = MemberSidebar.retryChannelSubscription(
				guildId,
				channelId,
				desiredRangesRef.current,
				ownerId,
				forceResend,
			);
			if (!reachedWire && !ownsSubscription()) {
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
			sendDesiredRequest();
		}

		function reconcileSubscription(): void {
			if (disposed || !gatewayAvailable()) {
				return;
			}
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
					phase = 'offline';
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
