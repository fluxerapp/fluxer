// SPDX-License-Identifier: AGPL-3.0-or-later

import assert from 'node:assert/strict';

export function assertNonEmptyString(value: unknown, fieldName: string): asserts value is string {
	assert.equal(typeof value, 'string', `${fieldName} must be a string`);
	assert.ok((value as string).length > 0, `${fieldName} must be a non-empty string`);
}

export function assertOptionalNonEmptyString(
	value: unknown,
	fieldName: string,
): asserts value is string | null | undefined {
	if (value === null || value === undefined) return;
	assert.equal(typeof value, 'string', `${fieldName} must be a string when provided`);
	assert.ok((value as string).length > 0, `${fieldName} must be a non-empty string when provided`);
}

export function assertString(value: unknown, name: string): asserts value is string {
	assert.equal(typeof value, 'string', `${name} must be string`);
}

export function assertPositiveFinite(value: unknown, fieldName: string): asserts value is number {
	assert.equal(typeof value, 'number', `${fieldName} must be a number`);
	assert.ok(Number.isFinite(value as number), `${fieldName} must be finite`);
	assert.ok((value as number) > 0, `${fieldName} must be positive`);
}

export function assertNonNegativeFinite(value: unknown, fieldName: string): asserts value is number {
	assert.equal(typeof value, 'number', `${fieldName} must be a number`);
	assert.ok(Number.isFinite(value as number), `${fieldName} must be finite`);
	assert.ok((value as number) >= 0, `${fieldName} must be non-negative`);
}

export function assertFiniteNumber(value: unknown, name: string): asserts value is number {
	assert.equal(typeof value, 'number', `${name} must be number`);
	assert.ok(Number.isFinite(value), `${name} must be finite`);
}

export function assertNonNegativeInteger(value: unknown, name: string): asserts value is number {
	assertFiniteNumber(value, name);
	assert.ok((value as number) >= 0, `${name} must be non-negative`);
	assert.equal(Math.trunc(value as number), value, `${name} must be integer`);
}

export function assertBoolean(value: unknown, fieldName: string): asserts value is boolean {
	assert.equal(typeof value, 'boolean', `${fieldName} must be a boolean`);
}

export function assertObjectLike<T extends object>(value: unknown, fieldName: string): asserts value is T {
	assert.ok(value !== null && typeof value === 'object', `${fieldName} must be a non-null object`);
}

export function assertNullableObjectLike<T extends object>(
	value: unknown,
	fieldName: string,
): asserts value is T | null {
	if (value === null) return;
	assert.ok(typeof value === 'object', `${fieldName} must be a non-null object or null`);
}

export function assertNonNullObject(value: unknown, name: string): void {
	assert.ok(value !== null && value !== undefined, `${name} must not be null/undefined`);
	assert.equal(typeof value, 'object', `${name} must be object`);
}

export function assertFunctionLike(
	value: unknown,
	fieldName: string,
): asserts value is (...args: Array<unknown>) => unknown {
	assert.equal(typeof value, 'function', `${fieldName} must be a function`);
}

export function assertFunction(value: unknown, name: string): asserts value is (...args: Array<unknown>) => unknown {
	assert.equal(typeof value, 'function', `${name} must be function`);
}

export function assertBoundedSize(size: number, cap: number, name: string): void {
	assertNonNegativeInteger(size, `${name}.size`);
	assert.ok(size <= cap, `${name} exceeded cap=${cap} (size=${size})`);
}

export function assertMonotonicForward(prevMs: number, nextMs: number, name: string): void {
	assertFiniteNumber(prevMs, `${name}.prevMs`);
	assertFiniteNumber(nextMs, `${name}.nextMs`);
	assert.ok(nextMs >= prevMs, `${name} must be monotonic forward`);
}

export interface MutedOrDeafenedSnapshot {
	readonly serverMute: boolean;
	readonly serverDeaf: boolean;
	readonly selfDeaf: boolean;
}

export function isMutedOrDeafened(state: MutedOrDeafenedSnapshot): boolean {
	if (state.serverMute) return true;
	if (state.serverDeaf) return true;
	if (state.selfDeaf) return true;
	return false;
}

export function isPermissionDeniedError(error: unknown): boolean {
	if (!(error instanceof Error)) return false;
	if (error.name === 'NotAllowedError') return true;
	if (error.name === 'PermissionDeniedError') return true;
	return false;
}

export interface VoiceServerUpdateLike {
	readonly token: string;
	readonly endpoint: string;
	readonly connection_id: string;
	readonly guild_id?: string | null;
	readonly channel_id?: string | null;
	readonly e2ee_key?: string | null;
}

export function assertVoiceServerUpdateShape(raw: unknown, fieldName: string): asserts raw is VoiceServerUpdateLike {
	assertObjectLike<VoiceServerUpdateLike>(raw, fieldName);
	const candidate = raw as unknown as Record<string, unknown>;
	assert.equal(typeof candidate.token, 'string', `${fieldName}.token must be a string`);
	assert.equal(typeof candidate.endpoint, 'string', `${fieldName}.endpoint must be a string`);
	assert.equal(typeof candidate.connection_id, 'string', `${fieldName}.connection_id must be a string`);
	if (candidate.guild_id !== undefined && candidate.guild_id !== null) {
		assert.equal(typeof candidate.guild_id, 'string', `${fieldName}.guild_id must be a string when provided`);
	}
	if (candidate.channel_id !== undefined && candidate.channel_id !== null) {
		assert.equal(typeof candidate.channel_id, 'string', `${fieldName}.channel_id must be a string when provided`);
	}
	if (candidate.e2ee_key !== undefined && candidate.e2ee_key !== null) {
		assert.equal(typeof candidate.e2ee_key, 'string', `${fieldName}.e2ee_key must be a string when provided`);
	}
}

export function assertDisconnectReason(
	reason: unknown,
	fieldName: string,
): asserts reason is 'user' | 'error' | 'server' {
	assert.equal(typeof reason, 'string', `${fieldName} must be a string`);
	const value = reason as string;
	if (value === 'user') return;
	if (value === 'error') return;
	if (value === 'server') return;
	assert.fail(`${fieldName} must be one of 'user' | 'error' | 'server'`);
}

export interface TerminalTransportSnapshot {
	readonly current: {readonly room?: unknown};
	readonly hotSwap: {readonly pendingRoom?: unknown; readonly previousRoom?: unknown};
}

export function hasAnyTerminalTransport(snapshot: TerminalTransportSnapshot): boolean {
	if (snapshot.current.room) return true;
	if (snapshot.hotSwap.pendingRoom) return true;
	if (snapshot.hotSwap.previousRoom) return true;
	return false;
}

export interface RepublishableTrack {
	readonly mediaStreamTrack?: {readonly readyState?: string} | null;
}

export function isReadyToRepublishTrack<T extends RepublishableTrack>(
	track: T | null | undefined,
): track is T & {mediaStreamTrack: {readonly readyState: string}} {
	if (!track) return false;
	const stream = track.mediaStreamTrack;
	if (!stream) return false;
	if (stream.readyState === 'ended') return false;
	return true;
}
