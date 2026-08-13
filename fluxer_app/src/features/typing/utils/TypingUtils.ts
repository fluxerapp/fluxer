// SPDX-License-Identifier: AGPL-3.0-or-later

import Authentication from '@app/features/auth/state/Authentication';
import * as TypingCommands from '@app/features/typing/commands/TypingCommands';
import {
	createLocalTypingSnapshot,
	type LocalTypingMachineEvent,
	type LocalTypingModel,
	type LocalTypingSnapshot,
	selectLocalTypingModel,
	transitionLocalTypingSnapshot,
} from '@app/features/typing/state/LocalTypingStateMachine';
import TypingIndicator from '@app/features/typing/state/TypingIndicator';

class TypingManager {
	private snapshot: LocalTypingSnapshot = createLocalTypingSnapshot();
	private remoteSendTimerId: NodeJS.Timeout | null = null;
	private localIdleTimerId: NodeJS.Timeout | null = null;
	private scheduledRemoteSendAt: number | null = null;
	private scheduledRemoteChannelId: string | null = null;
	private scheduledRemotePendingVersion: number | null = null;
	private scheduledLocalIdleAt: number | null = null;
	private scheduledLocalIdleChannelId: string | null = null;
	private remoteSendTimerGeneration = 0;
	private localIdleTimerGeneration = 0;

	typing(channelId: string): void {
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) {
			return;
		}
		this.transition(
			{
				type: 'localTyping.started',
				channelId,
				now: Date.now(),
			},
			currentUserId,
		);
	}

	clear(channelId: string): void {
		const currentUserId = Authentication.currentUserId;
		this.transition(
			{
				type: 'localTyping.stopped',
				channelId,
			},
			currentUserId,
		);
	}

	reset(): void {
		this.clearRemoteSendTimer();
		this.clearLocalIdleTimer();
		this.snapshot = createLocalTypingSnapshot();
	}

	private transition(event: LocalTypingMachineEvent, currentUserId: string | null): void {
		const previousModel = selectLocalTypingModel(this.snapshot);
		this.snapshot = transitionLocalTypingSnapshot(this.snapshot, event);
		const nextModel = selectLocalTypingModel(this.snapshot);
		if (currentUserId != null) {
			this.applyLocalTypingMutation(previousModel, nextModel, currentUserId, event);
		}
		this.applyRemoteSendSchedule(nextModel);
		this.applyLocalIdleSchedule(nextModel);
	}

	private applyLocalTypingMutation(
		previousModel: LocalTypingModel,
		nextModel: LocalTypingModel,
		userId: string,
		event: LocalTypingMachineEvent,
	): void {
		if (previousModel.localTyping && previousModel.channelId && previousModel.channelId !== nextModel.channelId) {
			TypingCommands.stopLocalTyping(previousModel.channelId, userId);
		}
		if (previousModel.localTyping && !nextModel.localTyping && previousModel.channelId) {
			TypingCommands.stopLocalTyping(previousModel.channelId, userId);
			return;
		}
		if (event.type === 'localTyping.started' && nextModel.localTyping && nextModel.channelId) {
			TypingCommands.startLocalTyping(nextModel.channelId, userId);
		}
	}

	private applyRemoteSendSchedule(nextModel: LocalTypingModel): void {
		if (
			nextModel.remoteSendAt === this.scheduledRemoteSendAt &&
			nextModel.channelId === this.scheduledRemoteChannelId &&
			nextModel.remotePendingVersion === this.scheduledRemotePendingVersion
		) {
			return;
		}
		this.clearRemoteSendTimer();
		if (nextModel.remoteSendAt == null || !nextModel.channelId) {
			return;
		}
		const channelId = nextModel.channelId;
		const pendingVersion = nextModel.remotePendingVersion;
		const remoteSendAt = nextModel.remoteSendAt;
		const timerGeneration = this.remoteSendTimerGeneration;
		this.scheduledRemoteSendAt = remoteSendAt;
		this.scheduledRemoteChannelId = channelId;
		this.scheduledRemotePendingVersion = pendingVersion;
		this.remoteSendTimerId = setTimeout(
			() => this.sendTyping(channelId, pendingVersion, timerGeneration),
			Math.max(0, remoteSendAt - Date.now()),
		);
	}

	private applyLocalIdleSchedule(nextModel: LocalTypingModel): void {
		if (
			nextModel.localIdleAt === this.scheduledLocalIdleAt &&
			nextModel.channelId === this.scheduledLocalIdleChannelId
		) {
			return;
		}
		this.clearLocalIdleTimer();
		if (nextModel.localIdleAt == null || !nextModel.channelId) {
			return;
		}
		const channelId = nextModel.channelId;
		const localIdleAt = nextModel.localIdleAt;
		const timerGeneration = this.localIdleTimerGeneration;
		this.scheduledLocalIdleAt = localIdleAt;
		this.scheduledLocalIdleChannelId = channelId;
		this.localIdleTimerId = setTimeout(
			() => this.expireLocalTyping(channelId, timerGeneration),
			Math.max(0, localIdleAt - Date.now()),
		);
	}

	private sendTyping(channelId: string, pendingVersion: number, timerGeneration: number): void {
		if (timerGeneration !== this.remoteSendTimerGeneration) {
			return;
		}
		this.remoteSendTimerId = null;
		this.scheduledRemoteSendAt = null;
		this.scheduledRemoteChannelId = null;
		this.scheduledRemotePendingVersion = null;
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) {
			this.clear(channelId);
			return;
		}
		TypingCommands.sendTyping(channelId);
		TypingCommands.startLocalTyping(channelId, currentUserId);
		this.transition(
			{
				type: 'localTyping.remoteSent',
				channelId,
				now: Date.now(),
				pendingVersion,
			},
			currentUserId,
		);
	}

	private expireLocalTyping(channelId: string, timerGeneration: number): void {
		if (timerGeneration !== this.localIdleTimerGeneration) {
			return;
		}
		this.localIdleTimerId = null;
		this.scheduledLocalIdleAt = null;
		this.scheduledLocalIdleChannelId = null;
		const currentUserId = Authentication.currentUserId;
		if (!currentUserId) {
			this.clear(channelId);
			return;
		}
		this.transition(
			{
				type: 'localTyping.idleElapsed',
				channelId,
				now: Date.now(),
			},
			currentUserId,
		);
	}

	private clearRemoteSendTimer(): void {
		this.remoteSendTimerGeneration += 1;
		if (this.remoteSendTimerId != null) {
			clearTimeout(this.remoteSendTimerId);
			this.remoteSendTimerId = null;
		}
		this.scheduledRemoteSendAt = null;
		this.scheduledRemoteChannelId = null;
		this.scheduledRemotePendingVersion = null;
	}

	private clearLocalIdleTimer(): void {
		this.localIdleTimerGeneration += 1;
		if (this.localIdleTimerId != null) {
			clearTimeout(this.localIdleTimerId);
			this.localIdleTimerId = null;
		}
		this.scheduledLocalIdleAt = null;
		this.scheduledLocalIdleChannelId = null;
	}
}

export const TypingUtils = new TypingManager();
TypingIndicator.registerLocalTypingResetOwner(TypingUtils);
