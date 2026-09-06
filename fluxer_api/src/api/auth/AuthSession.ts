// SPDX-License-Identifier: AGPL-3.0-or-later

import {BotUserAuthSessionCreationDeniedError} from '@fluxer/errors/src/domains/auth/BotUserAuthSessionCreationDeniedError';
import {RegistrationPendingApprovalError} from '@fluxer/errors/src/domains/auth/RegistrationPendingApprovalError';
import {RegistrationRejectedError} from '@fluxer/errors/src/domains/auth/RegistrationRejectedError';
import {SessionTokenMismatchError} from '@fluxer/errors/src/domains/auth/SessionTokenMismatchError';
import {InvalidTokenError} from '@fluxer/errors/src/domains/core/InvalidTokenError';
import {UnknownUserError} from '@fluxer/errors/src/domains/user/UnknownUserError';
import {requireClientIp} from '@fluxer/ip_utils/src/ClientIp';
import type {AuthSessionResponse} from '@fluxer/schema/src/domains/auth/AuthSchemas';
import type {ApiContext} from '../ApiContext';
import type {UserID} from '../BrandedTypes';
import {REGISTRATION_PENDING_APPROVAL_TRAIT, REGISTRATION_REJECTED_TRAIT} from '../instance/InstanceConfigRepository';
import {Logger} from '../Logger';
import type {AuthSession} from '../models/AuthSession';
import type {User} from '../models/User';
import {lookupGeoip} from '../utils/IpUtils';
import {isFluxerNativeUserAgent, parseReportedClientOs} from '../utils/SessionClientIdentity';
import {mapAuthSessionsToResponse} from './AuthModel';
import * as AuthUtility from './AuthUtility';

export interface SessionOrigin {
	ip: string;
	userAgent: string | null;
	clientOs: string | null;
}

interface CreateAuthSessionParams {
	user: User;
	origin: SessionOrigin;
}

interface LogoutAuthSessionsParams {
	user: User;
	sessionIdHashes: Array<string>;
}

interface DispatchAuthSessionChangeParams {
	userId: UserID;
	oldAuthSessionIdHash: string;
	newAuthSessionIdHash: string;
	newToken: string;
}

interface ReplaceCurrentAuthSessionParams {
	user: User;
	currentAuthSession: AuthSession;
	request: Request;
}

interface ReplaceCurrentAuthSessionResult {
	token: string;
	authSession: AuthSession;
	oldAuthSessionIdHash: string;
	newAuthSessionIdHash: string;
}

interface CreateAdditionalAuthSessionFromTokenParams {
	token: string;
	expectedUserId?: string;
	origin: SessionOrigin;
}

export function resolveSessionOrigin(ctx: ApiContext, request: Request): SessionOrigin {
	const {config} = ctx.services;
	const ip = requireClientIp(request, {
		trustClientIpHeader: config.proxy.trust_client_ip_header,
		clientIpHeaderName: config.proxy.client_ip_header,
	});
	const userAgent = request.headers.get('user-agent')?.trim() || null;
	const clientOs = isFluxerNativeUserAgent(userAgent)
		? parseReportedClientOs(request.headers.get('x-fluxer-client-properties'))
		: null;
	return {ip, userAgent, clientOs};
}

export async function createAuthSession(
	ctx: ApiContext,
	{user, origin}: CreateAuthSessionParams,
): Promise<[token: string, AuthSession]> {
	const {users} = ctx.services;
	if (user.isBot) throw new BotUserAuthSessionCreationDeniedError();
	if (user.traits.has(REGISTRATION_PENDING_APPROVAL_TRAIT)) throw new RegistrationPendingApprovalError();
	if (user.traits.has(REGISTRATION_REJECTED_TRAIT)) throw new RegistrationRejectedError();
	user = await AuthUtility.handleBanStatus(ctx, user);
	const now = new Date();
	const token = await AuthUtility.generateAuthToken(ctx);
	let clientCountry: string | null = null;
	try {
		const geoip = await lookupGeoip(origin.ip);
		clientCountry = geoip.countryCode ? geoip.countryCode.toUpperCase() : null;
	} catch (error) {
		Logger.warn({userId: user.id.toString(), error}, 'GeoIP lookup failed at session creation');
	}
	const authSession = await users.createAuthSession({
		user_id: user.id,
		session_id_hash: Buffer.from(AuthUtility.getTokenIdHash(ctx, token)),
		created_at: now,
		approx_last_used_at: now,
		client_ip: origin.ip,
		client_user_agent: origin.userAgent,
		client_os: origin.clientOs,
		client_country: clientCountry,
		version: 1,
	});
	return [token, authSession];
}

export async function createAdditionalAuthSessionFromToken(
	ctx: ApiContext,
	{token, expectedUserId, origin}: CreateAdditionalAuthSessionFromTokenParams,
): Promise<{
	token: string;
	userId: string;
}> {
	const existingSession = await getAuthSessionByToken(ctx, token);
	if (!existingSession) {
		throw new InvalidTokenError();
	}
	const {users} = ctx.services;
	const user = await users.findUnique(existingSession.userId);
	if (!user) {
		throw new UnknownUserError();
	}
	if (expectedUserId && user.id.toString() !== expectedUserId) {
		throw new SessionTokenMismatchError();
	}
	const [newToken] = await createAuthSession(ctx, {user, origin});
	return {token: newToken, userId: user.id.toString()};
}

export async function getAuthSessionByToken(ctx: ApiContext, token: string): Promise<AuthSession | null> {
	const {users} = ctx.services;
	return users.getAuthSessionByToken(Buffer.from(AuthUtility.getTokenIdHash(ctx, token)));
}

export async function getAuthSessions(ctx: ApiContext, userId: UserID): Promise<Array<AuthSessionResponse>> {
	const {users} = ctx.services;
	const authSessions = await users.listAuthSessions(userId);
	return await mapAuthSessionsToResponse({authSessions});
}

export async function updateAuthSessionLastUsed(ctx: ApiContext, tokenHash: Uint8Array): Promise<void> {
	await ctx.services.userActivityBuffer.recordAuthSessionActivity(Buffer.from(tokenHash), new Date());
}

export async function revokeToken(ctx: ApiContext, token: string): Promise<void> {
	const {users, gateway} = ctx.services;
	const tokenHash = Buffer.from(AuthUtility.getTokenIdHash(ctx, token));
	const authSession = await users.getAuthSessionByToken(tokenHash);
	if (!authSession) return;
	const sessionIdHash = Buffer.from(authSession.sessionIdHash).toString('base64url');
	await users.deletePushSubscriptionsForAuthSessions(authSession.userId, [sessionIdHash], {
		deleteUnboundSubscriptions: true,
	});
	await gateway.invalidatePushSubscriptions({userId: authSession.userId});
	await users.revokeAuthSession(tokenHash);
	await gateway.terminateSession({
		userId: authSession.userId,
		sessionIdHashes: [sessionIdHash],
	});
}

export async function logoutAuthSessions(
	ctx: ApiContext,
	{user, sessionIdHashes}: LogoutAuthSessionsParams,
): Promise<void> {
	const {users, gateway} = ctx.services;
	const hashes = sessionIdHashes.map((hash) => Buffer.from(hash, 'base64url'));
	await users.deletePushSubscriptionsForAuthSessions(user.id, sessionIdHashes, {
		deleteUnboundSubscriptions: true,
	});
	await gateway.invalidatePushSubscriptions({userId: user.id});
	await users.deleteAuthSessions(user.id, hashes);
	await gateway.terminateSession({
		userId: user.id,
		sessionIdHashes,
	});
}

export async function terminateAllUserSessions(ctx: ApiContext, userId: UserID): Promise<number> {
	const {users, gateway} = ctx.services;
	const authSessions = await users.listAuthSessions(userId);
	await users.deleteAllPushSubscriptions(userId);
	await gateway.invalidatePushSubscriptions({userId});
	if (authSessions.length === 0) return 0;
	const hashes = authSessions.map((s) => s.sessionIdHash);
	await users.deleteAuthSessions(userId, hashes);
	await gateway.terminateSession({
		userId,
		sessionIdHashes: authSessions.map((s) => Buffer.from(s.sessionIdHash).toString('base64url')),
	});
	return authSessions.length;
}

export async function replaceCurrentAuthSession(
	ctx: ApiContext,
	{user, currentAuthSession, request}: ReplaceCurrentAuthSessionParams,
): Promise<ReplaceCurrentAuthSessionResult> {
	const {users} = ctx.services;
	const oldAuthSessionIdHash = encodeSessionIdHash(currentAuthSession.sessionIdHash);
	const authSessions = await users.listAuthSessions(user.id);
	const otherAuthSessions = authSessions.filter(
		(authSession) => !authSession.sessionIdHash.equals(currentAuthSession.sessionIdHash),
	);
	await deleteAndTerminateAuthSessions(ctx, user.id, otherAuthSessions);
	const [newToken, newAuthSession] = await createAuthSession(ctx, {user, origin: resolveSessionOrigin(ctx, request)});
	const newAuthSessionIdHash = encodeSessionIdHash(newAuthSession.sessionIdHash);
	await dispatchAuthSessionChange(ctx, {
		userId: user.id,
		oldAuthSessionIdHash,
		newAuthSessionIdHash,
		newToken,
	});
	await deleteAndTerminateAuthSessions(ctx, user.id, [currentAuthSession]);
	return {
		token: newToken,
		authSession: newAuthSession,
		oldAuthSessionIdHash,
		newAuthSessionIdHash,
	};
}

async function deleteAndTerminateAuthSessions(
	ctx: ApiContext,
	userId: UserID,
	authSessions: ReadonlyArray<AuthSession>,
): Promise<void> {
	if (authSessions.length === 0) {
		return;
	}
	const {users, gateway} = ctx.services;
	const sessionIdHashes = authSessions.map((authSession) => encodeSessionIdHash(authSession.sessionIdHash));
	await users.deletePushSubscriptionsForAuthSessions(userId, sessionIdHashes, {
		deleteUnboundSubscriptions: true,
	});
	await gateway.invalidatePushSubscriptions({userId});
	await users.deleteAuthSessions(
		userId,
		authSessions.map((authSession) => authSession.sessionIdHash),
	);
	await gateway.terminateSession({
		userId,
		sessionIdHashes,
	});
}

function encodeSessionIdHash(sessionIdHash: Uint8Array): string {
	return Buffer.from(sessionIdHash).toString('base64url');
}

async function dispatchAuthSessionChange(ctx: ApiContext, params: DispatchAuthSessionChangeParams): Promise<void> {
	const {gateway} = ctx.services;
	const {userId, oldAuthSessionIdHash, newAuthSessionIdHash, newToken} = params;
	await gateway.dispatchPresence({
		userId,
		event: 'AUTH_SESSION_CHANGE',
		data: {
			old_auth_session_id_hash: oldAuthSessionIdHash,
			new_auth_session_id_hash: newAuthSessionIdHash,
			new_token: newToken,
		},
	});
}
