// SPDX-License-Identifier: AGPL-3.0-or-later

import GatewaySessions from '@app/features/gateway/transport/GatewaySessionPool';
import type {GatewaySessionRetirementReason} from '@app/features/gateway/transport/GatewaySessionRetirement';
import type {GatewaySocket, GatewayVoiceStateUpdateParams} from '@app/features/gateway/transport/GatewaySocket';
import {AccountScopedWorkTransitionReason} from '@app/features/platform/state/AccountScopedWork';

class GatewayConnection {
	get socket(): GatewaySocket | null {
		return GatewaySessions.foregroundConnection.socket;
	}

	get isConnected(): boolean {
		return GatewaySessions.foregroundConnection.isConnected;
	}

	get connectionEpoch(): number {
		return GatewaySessions.foregroundConnection.connectionEpoch;
	}

	get isConnecting(): boolean {
		return GatewaySessions.foregroundConnection.isConnecting;
	}

	get isReady(): boolean {
		return GatewaySessions.foregroundConnection.isReady;
	}

	get isConnectionInterrupted(): boolean {
		return GatewaySessions.foregroundConnection.isConnectionInterrupted;
	}

	get sessionId(): string | null {
		return GatewaySessions.foregroundConnection.sessionId;
	}

	get foregroundAccountKey(): string | null {
		return GatewaySessions.foregroundConnection.foregroundAccountKey;
	}

	waitForForegroundReady(accountKey: string | null): Promise<void> {
		return GatewaySessions.waitForForegroundReady(accountKey);
	}

	recoverForegroundSession(accountKey: string): void {
		GatewaySessions.recoverForegroundSession(accountKey);
	}

	async beginForegroundPromotion(accountKey: string): Promise<void> {
		await GatewaySessions.beginPromote(accountKey);
	}

	async completeForegroundPromotion(accountKey: string): Promise<void> {
		await GatewaySessions.completePromote(accountKey);
	}

	async rollbackForegroundPromotion(accountKey: string): Promise<void> {
		await GatewaySessions.rollbackPromote(accountKey);
	}

	setToken(token: string): void {
		GatewaySessions.foregroundConnection.setToken(token);
	}

	reuseReadyForegroundSession(accountKey: string, token: string): void {
		GatewaySessions.foregroundConnection.reuseReadySession(accountKey, token);
	}

	syncGuildIfNeeded(guildId: string, reason?: string, force = false): void {
		GatewaySessions.foregroundConnection.syncGuildIfNeeded(guildId, reason, force);
	}

	hasCompletedGuildSync(guildId: string): boolean {
		return GatewaySessions.foregroundConnection.hasCompletedGuildSync(guildId);
	}

	sendTerminalVoiceDisconnect(params: GatewayVoiceStateUpdateParams, reason: string): boolean {
		return GatewaySessions.foregroundConnection.sendTerminalVoiceDisconnect(params, reason);
	}

	sendInvisiblePresenceForCurrentSession(reason: GatewaySessionRetirementReason): void {
		GatewaySessions.foregroundConnection.sendInvisiblePresenceForCurrentSession(reason);
	}

	logout(): void {
		GatewaySessions.foregroundConnection.logout();
	}

	async retireForAccountTransition(reason: AccountScopedWorkTransitionReason): Promise<void> {
		if (reason === AccountScopedWorkTransitionReason.ACCOUNT_SWITCH) {
			await GatewaySessions.demoteForegroundForAccountSwitch();
			return;
		}
		GatewaySessions.foregroundConnection.logout();
	}
}

export default new GatewayConnection();
