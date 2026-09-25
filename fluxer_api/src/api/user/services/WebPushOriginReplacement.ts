// SPDX-License-Identifier: AGPL-3.0-or-later

import type {IKVProvider} from '@pkgs/kv_client/src/IKVProvider';
import {seconds} from 'itty-time';
import {uint8ArrayToBase64} from 'uint8array-extras';

export type WebPushOriginKind = 'legacy' | 'target';

export type WebPushOriginReplacement = 'installed' | 'browser';

const WEB_PUSH_ORIGIN_KINDS: ReadonlyMap<string, WebPushOriginKind> = new Map([
	['https://web.fluxer.app', 'legacy'],
	['https://web.canary.fluxer.app', 'legacy'],
	['https://fluxer.com', 'target'],
	['https://canary.fluxer.com', 'target'],
]);

const PUSH_ORIGIN_REPLACED_PREFIX = 'push_origin_replaced:';
const PUSH_SESSION_PREDECESSOR_PREFIX = 'push_session_predecessor:';
const PUSH_TARGET_SUBSCRIPTION_PREFIX = 'push_target_subscription:';
const PUSH_INSTALLED_LEGACY_SUBSCRIPTION_PREFIX = 'push_installed_legacy_subscription:';
const USER_AGENT_VERSION_PATTERN = /\d+(?:[._]\d+)*/g;

export const WEB_PUSH_ORIGIN_RECORD_TTL_SECONDS = seconds('400 days');

export function classifyWebPushOrigin(
	origin: string | null | undefined,
	selfHosted: boolean,
): WebPushOriginKind | null {
	if (selfHosted || !origin) return null;
	return WEB_PUSH_ORIGIN_KINDS.get(origin) ?? null;
}

export function encodePushSessionIdHash(sessionIdHash: Uint8Array): string {
	return uint8ArrayToBase64(sessionIdHash, {urlSafe: true});
}

export function sameUserAgentFamily(a: string | null | undefined, b: string | null | undefined): boolean {
	if (!a || !b) return false;
	return a.replace(USER_AGENT_VERSION_PATTERN, '') === b.replace(USER_AGENT_VERSION_PATTERN, '');
}

export async function recordPushSessionPredecessor(
	kv: IKVProvider,
	sessionIdHash: string,
	predecessorSessionIdHash: string,
): Promise<void> {
	if (sessionIdHash === predecessorSessionIdHash) return;
	await kv.setex(
		`${PUSH_SESSION_PREDECESSOR_PREFIX}${sessionIdHash}`,
		WEB_PUSH_ORIGIN_RECORD_TTL_SECONDS,
		predecessorSessionIdHash,
	);
}

export async function getPushSessionPredecessor(kv: IKVProvider, sessionIdHash: string): Promise<string | null> {
	return kv.get(`${PUSH_SESSION_PREDECESSOR_PREFIX}${sessionIdHash}`);
}

export async function markPushOriginReplaced(
	kv: IKVProvider,
	sessionIdHash: string,
	replacement: WebPushOriginReplacement,
): Promise<void> {
	const key = `${PUSH_ORIGIN_REPLACED_PREFIX}${sessionIdHash}`;
	if (replacement === 'browser' && (await kv.get(key)) === 'installed') return;
	await kv.setex(key, WEB_PUSH_ORIGIN_RECORD_TTL_SECONDS, replacement);
}

export async function getPushOriginReplacement(
	kv: IKVProvider,
	sessionIdHash: string,
): Promise<WebPushOriginReplacement | null> {
	const value = await kv.get(`${PUSH_ORIGIN_REPLACED_PREFIX}${sessionIdHash}`);
	if (value === null) return null;
	return value === 'browser' ? 'browser' : 'installed';
}

async function markSubscription(kv: IKVProvider, prefix: string, subscriptionId: string): Promise<void> {
	await kv.setex(`${prefix}${subscriptionId}`, WEB_PUSH_ORIGIN_RECORD_TTL_SECONDS, '1');
}

async function findMarkedSubscriptionIds(
	kv: IKVProvider,
	prefix: string,
	subscriptionIds: Array<string>,
): Promise<Set<string>> {
	if (subscriptionIds.length === 0) return new Set();
	const markers = await kv.mget(...subscriptionIds.map((id) => `${prefix}${id}`));
	return new Set(subscriptionIds.filter((_, index) => markers[index] !== null));
}

export async function markTargetPushSubscription(kv: IKVProvider, subscriptionId: string): Promise<void> {
	await markSubscription(kv, PUSH_TARGET_SUBSCRIPTION_PREFIX, subscriptionId);
}

export async function findTargetPushSubscriptionIds(
	kv: IKVProvider,
	subscriptionIds: Array<string>,
): Promise<Set<string>> {
	return findMarkedSubscriptionIds(kv, PUSH_TARGET_SUBSCRIPTION_PREFIX, subscriptionIds);
}

export async function markInstalledLegacyPushSubscription(kv: IKVProvider, subscriptionId: string): Promise<void> {
	await markSubscription(kv, PUSH_INSTALLED_LEGACY_SUBSCRIPTION_PREFIX, subscriptionId);
}

export async function findInstalledLegacyPushSubscriptionIds(
	kv: IKVProvider,
	subscriptionIds: Array<string>,
): Promise<Set<string>> {
	return findMarkedSubscriptionIds(kv, PUSH_INSTALLED_LEGACY_SUBSCRIPTION_PREFIX, subscriptionIds);
}
