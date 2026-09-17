// SPDX-License-Identifier: AGPL-3.0-or-later

import {createUserID} from '@app/api/BrandedTypes';
import type {BlueskyOAuthGrantOwner} from '@app/api/bluesky/IBlueskyOAuthService';
import type {ConnectionCredentialRepository} from '@app/api/connection/ConnectionCredentialRepository';
import {
	type AtprotoDid,
	type AtprotoOAuthScope,
	isAtprotoDid,
	isAtprotoOAuthScope,
	type Jwk,
	jwkPrivateSchema,
	type NodeSavedSession,
	type NodeSavedSessionStore,
	type NodeSavedState,
	type NodeSavedStateStore,
} from '@atproto/oauth-client-node';
import {SnowflakeType} from '@fluxer/schema/src/primitives/SchemaPrimitives';
import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {z} from 'zod';

const STATE_LOCATOR_PREFIX = 'bsky:oauth:state_locator:';

const GrantOwnerSchema: z.ZodType<BlueskyOAuthGrantOwner> = z.strictObject({
	userId: z
		.string()
		.max(19)
		.regex(/^(0|[1-9]\d*)$/)
		.transform((value, ctx) => {
			const parsed = SnowflakeType.safeParse(value);
			if (!parsed.success) {
				ctx.addIssue({code: 'custom', message: 'Invalid user ID'});
				return z.NEVER;
			}
			return createUserID(parsed.data);
		}),
	grantId: z.uuid(),
});
const SavedAuthMethodSchema = z.strictObject({method: z.literal('private_key_jwt'), kid: z.string().min(1)});
const SavedKeySchema = z.custom<Jwk>((value) => jwkPrivateSchema.safeParse(value).success);
const SavedStateSchema: z.ZodType<NodeSavedState> = z.strictObject({
	iss: z.string(),
	dpopJwk: SavedKeySchema,
	authMethod: SavedAuthMethodSchema,
	verifier: z.string().min(1),
	appState: z.string(),
});
const SavedSessionSchema: z.ZodType<NodeSavedSession> = z.strictObject({
	dpopJwk: SavedKeySchema,
	authMethod: SavedAuthMethodSchema,
	tokenSet: z.strictObject({
		iss: z.string(),
		sub: z.custom<AtprotoDid>(isAtprotoDid),
		aud: z.string(),
		scope: z.custom<AtprotoOAuthScope>((value) => typeof value === 'string' && isAtprotoOAuthScope(value)),
		refresh_token: z.string().optional(),
		access_token: z.string(),
		token_type: z.literal('DPoP'),
		expires_at: z.iso.datetime().optional(),
	}),
});

type BlueskyOAuthStoreKind = 'state' | 'session';

export class BlueskyOAuthStateInvalidError extends Error {
	constructor() {
		super('Bluesky OAuth state is invalid');
	}
}

export class BlueskyOAuthStoreError extends Error {
	constructor(
		readonly kind: BlueskyOAuthStoreKind,
		message: string,
	) {
		super(message);
	}
}

function parseBlueskyGrantOwner(state: string | null | undefined): BlueskyOAuthGrantOwner {
	if (typeof state !== 'string' || state.length > 128) throw new BlueskyOAuthStateInvalidError();
	let value: unknown;
	try {
		value = JSON.parse(state);
	} catch {
		throw new BlueskyOAuthStateInvalidError();
	}
	const parsed = GrantOwnerSchema.safeParse(value);
	if (!parsed.success) throw new BlueskyOAuthStateInvalidError();
	return parsed.data;
}

export function validateBlueskyGrantOwner(owner: BlueskyOAuthGrantOwner): BlueskyOAuthGrantOwner {
	if (owner === null || typeof owner !== 'object' || typeof owner.userId !== 'bigint') {
		throw new BlueskyOAuthStoreError('session', 'Bluesky OAuth session owner is invalid');
	}
	const parsed = GrantOwnerSchema.safeParse({userId: String(owner.userId), grantId: owner.grantId});
	if (!parsed.success) {
		throw new BlueskyOAuthStoreError('session', 'Bluesky OAuth session owner is invalid');
	}
	return parsed.data;
}

function parseSavedValue<T>(
	data: string | undefined,
	schema: z.ZodType<T>,
	kind: BlueskyOAuthStoreKind,
): T | undefined {
	if (data === undefined) return undefined;
	let value: unknown;
	try {
		value = JSON.parse(data);
	} catch {
		throw new BlueskyOAuthStoreError(kind, `Stored Bluesky OAuth ${kind} is not valid JSON`);
	}
	const parsed = schema.safeParse(value);
	if (!parsed.success) {
		const path = parsed.error.issues[0]?.path.join('.') || 'root';
		throw new BlueskyOAuthStoreError(kind, `Stored Bluesky OAuth ${kind} has an invalid ${path}`);
	}
	return parsed.data;
}

export function createStateStore(
	kvClient: IKVProvider,
	credentials: ConnectionCredentialRepository,
	ttlSeconds: number,
	resolveOwner: () => BlueskyOAuthGrantOwner,
	onConsume?: (owner: BlueskyOAuthGrantOwner) => void,
): NodeSavedStateStore {
	return {
		async set(key: string, internalState: NodeSavedState): Promise<void> {
			const owner = validateBlueskyGrantOwner(resolveOwner());
			const stateOwner = parseBlueskyGrantOwner(internalState.appState);
			if (stateOwner.userId !== owner.userId || stateOwner.grantId !== owner.grantId) {
				throw new BlueskyOAuthStateInvalidError();
			}
			await credentials.createState(owner, key, JSON.stringify(internalState), ttlSeconds);
			await kvClient.setex(
				`${STATE_LOCATOR_PREFIX}${key}`,
				ttlSeconds,
				JSON.stringify({userId: String(owner.userId), grantId: owner.grantId}),
			);
		},
		async get(key: string): Promise<NodeSavedState | undefined> {
			const locator = await kvClient.getdel(`${STATE_LOCATOR_PREFIX}${key}`);
			if (locator === null) return undefined;
			const owner = parseBlueskyGrantOwner(locator);
			const state = parseSavedValue(await credentials.consumeState(owner, key), SavedStateSchema, 'state');
			if (state !== undefined) {
				const stateOwner = parseBlueskyGrantOwner(state.appState);
				if (stateOwner.userId !== owner.userId || stateOwner.grantId !== owner.grantId) {
					throw new BlueskyOAuthStateInvalidError();
				}
				onConsume?.(owner);
			}
			return state;
		},
		async del(key: string): Promise<void> {
			const locator = await kvClient.getdel(`${STATE_LOCATOR_PREFIX}${key}`);
			if (locator !== null) await credentials.discardPendingState(parseBlueskyGrantOwner(locator), key);
		},
	};
}

export function createSessionStore(
	credentials: ConnectionCredentialRepository,
	ttlSeconds: number,
	resolveOwner: () => BlueskyOAuthGrantOwner,
): NodeSavedSessionStore {
	function sessionOwner(sub: string): BlueskyOAuthGrantOwner {
		const owner = validateBlueskyGrantOwner(resolveOwner());
		if (!isAtprotoDid(sub)) {
			throw new BlueskyOAuthStoreError('session', 'Bluesky OAuth session identity is invalid');
		}
		return owner;
	}

	return {
		async set(sub: string, session: NodeSavedSession): Promise<void> {
			if (session.tokenSet.sub !== sub) {
				throw new BlueskyOAuthStoreError('session', 'Bluesky OAuth session does not match the requested identity');
			}
			await credentials.setSession(sessionOwner(sub), sub, JSON.stringify(session), ttlSeconds);
		},
		async get(sub: string): Promise<NodeSavedSession | undefined> {
			const owner = sessionOwner(sub);
			let data: string | undefined;
			try {
				data = await credentials.getSession(owner, sub);
			} catch {
				throw new BlueskyOAuthStoreError('session', 'Could not read stored Bluesky OAuth session');
			}
			const session = parseSavedValue(data, SavedSessionSchema, 'session');
			if (session !== undefined && session.tokenSet.sub !== sub) {
				throw new BlueskyOAuthStoreError(
					'session',
					'Stored Bluesky OAuth session does not match the requested identity',
				);
			}
			return session;
		},
		async del(sub: string): Promise<void> {
			await credentials.deleteSession(sessionOwner(sub), sub);
		},
	};
}
