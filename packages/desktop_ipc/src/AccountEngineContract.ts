// SPDX-License-Identifier: AGPL-3.0-or-later

export const ACCOUNT_ENGINE_CHANNELS = Object.freeze({
	connect: 'account-engine:connect',
	port: 'account-engine:port',
	accounts: 'account-engine:accounts',
} as const);

export const ACCOUNT_ENGINE_AVAILABLE_RENDERER_ARG = '--fluxer-account-engine=1';

export const ACCOUNT_ENGINE_LIMITS = Object.freeze({
	accountKeyMaxChars: 4096,
	endpointMaxChars: 2048,
	requestTimeoutMs: 30_000,
	promotionTimeoutMs: 15_000,
	activityChannelLimitMax: 200,
	activityMentionLimitMax: 100,
	mentionContentMaxChars: 2000,
	recipientsMax: 10,
	summaryCoalesceMs: 150,
	demotionCaptureMaxBytes: 256 * 1024 * 1024,
	providedAccountsMax: 1024,
} as const);

export const AccountEngineConnectionState = Object.freeze({
	STARTING: 'starting',
	CONNECTING: 'connecting',
	READY: 'ready',
	RESUMING: 'resuming',
	BACKOFF: 'backoff',
	OFFLINE: 'offline',
	AUTH_FAILED: 'auth_failed',
	FOREGROUND: 'foreground',
	STOPPED: 'stopped',
} as const);
export type AccountEngineConnectionState =
	(typeof AccountEngineConnectionState)[keyof typeof AccountEngineConnectionState];

export const AccountEngineCacheState = Object.freeze({EMPTY: 'empty', STALE: 'stale', LIVE: 'live'} as const);
export type AccountEngineCacheState = (typeof AccountEngineCacheState)[keyof typeof AccountEngineCacheState];

export interface AccountEngineUser {
	readonly id: string;
	readonly username: string;
	readonly globalName: string | null;
	readonly avatar: string | null;
	readonly discriminator: string | null;
}

export interface AccountEngineAccountSummary {
	readonly accountKey: string;
	readonly instanceKey: string;
	readonly userId: string;
	readonly user: AccountEngineUser | null;
	readonly state: AccountEngineConnectionState;
	readonly stateChangedAt: number;
	readonly nextRetryAt: number | null;
	readonly mentionCount: number;
	readonly unreadChannelCount: number;
	readonly lastMessageAt: number | null;
	readonly cache: AccountEngineCacheState;
	readonly cacheUpdatedAt: number | null;
	readonly degraded: boolean;
	readonly revision: number;
}

export interface AccountEngineChannelActivity {
	readonly channelId: string;
	readonly guildId: string | null;
	readonly parentId: string | null;
	readonly type: number;
	readonly name: string | null;
	readonly guildName: string | null;
	readonly guildIcon: string | null;
	readonly recipients: ReadonlyArray<AccountEngineUser>;
	readonly mentionCount: number;
	readonly unread: boolean;
	readonly lastMessageId: string | null;
	readonly ackMessageId: string | null;
}

export interface AccountEngineMention {
	readonly messageId: string;
	readonly channelId: string;
	readonly guildId: string | null;
	readonly channelType: number | null;
	readonly channelName: string | null;
	readonly guildName: string | null;
	readonly author: AccountEngineUser;
	readonly content: string;
	readonly gated: boolean;
	readonly timestamp: string;
}

export interface AccountEngineAccountActivity {
	readonly accountKey: string;
	readonly channels: ReadonlyArray<AccountEngineChannelActivity>;
	readonly totalChannelCount: number;
	readonly mentions: ReadonlyArray<AccountEngineMention>;
}

export interface AccountEngineNotification {
	readonly accountKey: string;
	readonly message: unknown;
	readonly guildName: string | null;
	readonly channelName: string | null;
	readonly channelType: number | null;
	readonly mentioned: boolean;
	readonly gated: boolean;
}

export interface AccountEngineResumeSession {
	readonly sessionId: string;
	readonly sequence: number;
	readonly gatewayUrl: string;
}

export interface AccountEngineSnapshotCursor {
	readonly sessionId: string | null;
	readonly schemaEpoch: number | null;
	readonly updatedAt: number;
}

export type AccountEngineSnapshotEntries = Record<string, Record<string, string>>;

export interface AccountEngineSnapshotCapture {
	readonly entries: AccountEngineSnapshotEntries;
	readonly cursor: AccountEngineSnapshotCursor;
}

export interface AccountEnginePromotion {
	readonly accountKey: string;
	readonly capture: AccountEngineSnapshotCapture | null;
	readonly mentionCounts: Readonly<Record<string, number>>;
	readonly session: AccountEngineResumeSession | null;
}

export interface AccountEngineGuildSubscriptionRelease {
	readonly member_list_channels?: Readonly<Record<string, readonly []>>;
	readonly members?: readonly [];
	readonly threads?: false;
	readonly thread_member_lists?: readonly [];
	readonly focused_channel_id?: null;
}

export type AccountEngineSubscriptionRelease = Readonly<Record<string, AccountEngineGuildSubscriptionRelease>>;

export interface AccountEngineDemotion {
	readonly accountKey: string;
	readonly session: AccountEngineResumeSession | null;
	readonly release: AccountEngineSubscriptionRelease | null;
	readonly capture: AccountEngineSnapshotCapture | null;
	readonly mentionCounts: Readonly<Record<string, number>> | null;
}

export interface AccountEngineClientConfig {
	readonly properties: Readonly<Record<string, unknown>>;
	readonly apiVersion: number;
	readonly identifyFlags: number;
}

export interface AccountEngineRequests {
	configure: {params: AccountEngineClientConfig; result: null};
	claimForeground: {params: {readonly accountKey: string | null}; result: null};
	getSummaries: {params: null; result: ReadonlyArray<AccountEngineAccountSummary>};
	getActivity: {
		params: {
			readonly accountKeys: ReadonlyArray<string> | null;
			readonly channelLimit: number;
			readonly mentionLimit: number;
		};
		result: ReadonlyArray<AccountEngineAccountActivity>;
	};
	beginPromotion: {params: {readonly accountKey: string}; result: AccountEnginePromotion | null};
	completePromotion: {params: {readonly accountKey: string}; result: null};
	abortPromotion: {params: {readonly accountKey: string}; result: null};
	demote: {params: AccountEngineDemotion; result: null};
	ackChannel: {
		params: {readonly accountKey: string; readonly channelId: string; readonly messageId: string};
		result: boolean;
	};
	reconnect: {params: {readonly accountKey: string}; result: null};
	setUnreadBadgeCustomization: {params: {readonly enabledAccountKeys: ReadonlyArray<string>}; result: null};
}

export type AccountEngineRequestMethod = keyof AccountEngineRequests;

export type AccountEngineEvent =
	| {readonly type: 'summaries'; readonly summaries: ReadonlyArray<AccountEngineAccountSummary>}
	| {readonly type: 'removed'; readonly accountKeys: ReadonlyArray<string>}
	| {readonly type: 'notification'; readonly notification: AccountEngineNotification}
	| {readonly type: 'channel-read'; readonly accountKey: string; readonly channelId: string}
	| {readonly type: 'auth-failed'; readonly accountKey: string; readonly tokenTag: string}
	| {readonly type: 'account-record-changed'; readonly accountKey: string}
	| {
			readonly type: 'account-token-rotated';
			readonly accountKey: string;
			readonly previousTokenTag: string;
			readonly token: string;
	  }
	| {readonly type: 'restarted'};

export const AccountEngineErrorCode = Object.freeze({
	UNKNOWN_ACCOUNT: 'unknown_account',
	NOT_READY: 'not_ready',
	BUSY: 'busy',
	INVALID_REQUEST: 'invalid_request',
	UNAVAILABLE: 'unavailable',
	TIMEOUT: 'timeout',
} as const);
export type AccountEngineErrorCode = (typeof AccountEngineErrorCode)[keyof typeof AccountEngineErrorCode];

export class AccountEngineRequestError extends Error {
	readonly code: AccountEngineErrorCode;
	constructor(code: AccountEngineErrorCode, message: string) {
		super(message);
		this.name = 'AccountEngineRequestError';
		this.code = code;
	}
}

export interface AccountEngineRendererAPI {
	readonly isAvailable: boolean;
	request<M extends AccountEngineRequestMethod>(
		method: M,
		params: AccountEngineRequests[M]['params'],
	): Promise<AccountEngineRequests[M]['result']>;
	onEvent(listener: (event: AccountEngineEvent) => void): () => void;
	provideAccounts(accounts: ReadonlyArray<AccountEngineProvidedAccount>): Promise<boolean>;
}

export interface AccountEngineProvidedAccount {
	readonly storageKey: string;
	readonly record: {
		readonly userId: string;
		readonly token: string;
		readonly isValid: boolean;
		readonly lastActive: number;
		readonly userData: {readonly username: string; readonly globalName: string | null} | null;
		readonly instance: {
			readonly apiEndpoint: string;
			readonly gatewayEndpoint: string;
			readonly apiCodeVersion: number;
		};
	};
}

export function accountEngineInstanceKey(accountKey: string): string | null {
	const index = accountKey.lastIndexOf('::');
	return index <= 0 ? null : accountKey.slice(0, index);
}

const USER_MENTION_PATTERN = /<@!?(\d+)>/g;

function mentionedUserName(value: unknown): string | null {
	if (typeof value !== 'object' || value === null) return null;
	const user = value as {member?: {nick?: unknown} | null; global_name?: unknown; username?: unknown};
	for (const name of [user.member?.nick, user.global_name, user.username]) {
		if (typeof name === 'string' && name.length > 0) return name;
	}
	return null;
}

export function inlineMentionedUserNames(content: string, mentions: unknown): string {
	if (!content || !Array.isArray(mentions) || mentions.length === 0) return content;
	const names = new Map<string, string>();
	for (const mention of mentions) {
		const id = (mention as {id?: unknown} | null)?.id;
		const name = mentionedUserName(mention);
		if (typeof id === 'string' && name !== null) names.set(id, name);
	}
	return content.replace(USER_MENTION_PATTERN, (token, userId: string) => {
		const name = names.get(userId);
		return name === undefined ? token : `@${name}`;
	});
}
