// SPDX-License-Identifier: AGPL-3.0-or-later

export const VoiceAccountExitReason = Object.freeze({
	ACCOUNT_SWITCH: 'account-switch',
	LOGOUT: 'logout',
	ACCOUNT_REMOVED: 'account-removed',
} as const);

export type VoiceAccountExitReason = (typeof VoiceAccountExitReason)[keyof typeof VoiceAccountExitReason];

export interface VoiceAccountIdentity {
	readonly accountKey: string;
	readonly userId: string;
}

export interface VoiceAccountExitRequest extends VoiceAccountIdentity {
	readonly reason: VoiceAccountExitReason;
}

export interface VoiceAccountLifecycle {
	leaveVoiceChannelForAccountExit(request: VoiceAccountExitRequest): Promise<void>;
	suspendVoiceForAccountRestriction(account: VoiceAccountIdentity): Promise<void>;
}

function loadVoiceAccountLifecycle(): Promise<VoiceAccountLifecycle> {
	return import('@app/features/voice/VoiceAccountLifecycle');
}

export const voiceAccountLifecycle: VoiceAccountLifecycle = {
	async leaveVoiceChannelForAccountExit(request) {
		const lifecycle = await loadVoiceAccountLifecycle();
		await lifecycle.leaveVoiceChannelForAccountExit(request);
	},
	async suspendVoiceForAccountRestriction(account) {
		const lifecycle = await loadVoiceAccountLifecycle();
		await lifecycle.suspendVoiceForAccountRestriction(account);
	},
};
