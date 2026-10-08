// SPDX-License-Identifier: AGPL-3.0-or-later

import InstanceSnapshotStore, {
	type RuntimeConfigSnapshot,
	runtimeInstanceKey,
} from '@app/features/app/state/InstanceSnapshotStore';
import RuntimeConfig from '@app/features/app/state/RuntimeConfig';
import {desktopLocalApiEndpoint, isDesktopLocalAppDocument} from '@app/features/platform/DesktopLocalAppRuntime';
import type {AccountScopedWorkSuspension} from '@app/features/platform/state/AccountScopedWork';
import {createBrowserInstanceHTTP} from '@app/features/platform/transport/BrowserInstanceHTTP';
import {http} from '@app/features/platform/transport/RestTransport';
import type {
	HttpMethod,
	RestAuthMode,
	RestInterceptor,
	RestResponse,
} from '@app/features/platform/types/TransportTypes';
import {Logger} from '@app/features/platform/utils/AppLogger';

const logger = Logger.create('InstanceHTTP');

export type InstanceHTTPKind = 'browser';

export interface InstanceHTTPTarget {
	readonly instanceKey: string;
	readonly apiEndpoint: string;
	readonly apiVersion: number;
}

export interface InstanceHTTPRequest {
	readonly method: HttpMethod;
	readonly path: string;
	readonly target: InstanceHTTPTarget;
	readonly body?: unknown;
	readonly headers?: Record<string, string>;
	readonly auth?: RestAuthMode;
	readonly retries?: number;
	readonly timeoutMs?: number;
	readonly signal?: AbortSignal;
	readonly intercept?: RestInterceptor;
}

export interface InstanceHTTPDelivery {
	readonly method: HttpMethod;
	readonly path: string;
	readonly target: InstanceHTTPTarget;
	readonly body?: unknown;
	readonly headers?: Record<string, string>;
	readonly auth?: RestAuthMode;
	readonly retries?: number;
	readonly timeoutMs?: number;
	readonly signal?: AbortSignal;
	readonly intercept?: RestInterceptor;
	readonly accountScopedWork:
		| {readonly kind: 'standard'}
		| {readonly kind: 'account-transition'; readonly suspension: AccountScopedWorkSuspension};
}

export interface InstanceHTTP {
	readonly kind: InstanceHTTPKind;
	send<T>(delivery: InstanceHTTPDelivery): Promise<RestResponse<T>>;
}

export class InvalidInstanceHTTPTargetError extends Error {
	constructor(message: string) {
		super(message);
		this.name = 'InvalidInstanceHTTPTargetError';
	}
}

function withoutTrailingSlashes(value: string): string {
	return value.replace(/\/+$/u, '');
}

export function instanceTargetFromSnapshot(snapshot: RuntimeConfigSnapshot): InstanceHTTPTarget {
	const instanceKey = runtimeInstanceKey(snapshot);
	if (instanceKey === null) {
		throw new InvalidInstanceHTTPTargetError(`Runtime snapshot has an invalid API endpoint: ${snapshot.apiEndpoint}`);
	}
	const target = {
		instanceKey,
		apiEndpoint: snapshot.apiEndpoint,
		apiVersion: snapshot.apiCodeVersion,
	};
	validateInstanceTarget(target);
	return target;
}

function validateInstanceTarget(target: InstanceHTTPTarget): void {
	const endpointKey = runtimeInstanceKey({apiEndpoint: target.apiEndpoint});
	if (endpointKey === null) {
		throw new InvalidInstanceHTTPTargetError(`Instance target has an invalid API endpoint: ${target.apiEndpoint}`);
	}
	if (target.instanceKey !== endpointKey) {
		throw new InvalidInstanceHTTPTargetError(
			`Instance target key ${target.instanceKey} does not match API endpoint ${target.apiEndpoint}`,
		);
	}
	if (!Number.isSafeInteger(target.apiVersion) || target.apiVersion <= 0) {
		throw new InvalidInstanceHTTPTargetError(`Instance target has an invalid API version: ${target.apiVersion}`);
	}
}

export function instanceTargetIdentity(target: InstanceHTTPTarget): string {
	validateInstanceTarget(target);
	return JSON.stringify([target.instanceKey, target.apiVersion]);
}

export function currentInstanceTarget(): InstanceHTTPTarget {
	return instanceTargetFromSnapshot(RuntimeConfig.getSnapshot());
}

export function composeInstanceRequestPath(target: InstanceHTTPTarget, path: string): string {
	validateInstanceTarget(target);
	if (http.matchesConfiguredRouting(target.apiEndpoint, target.apiVersion)) {
		return path;
	}
	if (isDesktopLocalAppDocument()) {
		return `${withoutTrailingSlashes(desktopLocalApiEndpoint(target.instanceKey))}/v${target.apiVersion}${path}`;
	}
	return `${withoutTrailingSlashes(target.apiEndpoint)}/v${target.apiVersion}${path}`;
}

let selected: InstanceHTTP | null = null;

export function selectedInstanceHTTP(): InstanceHTTP {
	selected ??= createBrowserInstanceHTTP();
	return selected;
}

async function ensureDesktopRuntimeRoute(target: InstanceHTTPTarget, signal: AbortSignal | undefined): Promise<void> {
	try {
		await InstanceSnapshotStore.resolve({input: target.apiEndpoint, signal: signal ?? null});
	} catch (error) {
		signal?.throwIfAborted();
		logger.warn('Could not prepare the desktop route for an instance request', {
			instanceKey: target.instanceKey,
			error,
		});
	}
}

async function sendInstanceRequest<T>(
	request: InstanceHTTPRequest,
	accountScopedWork: InstanceHTTPDelivery['accountScopedWork'],
): Promise<RestResponse<T>> {
	const target = request.target;
	const path = composeInstanceRequestPath(target, request.path);
	const crossInstance = path !== request.path;
	if (crossInstance && isDesktopLocalAppDocument()) {
		await ensureDesktopRuntimeRoute(target, request.signal);
	}
	return await selectedInstanceHTTP().send<T>({
		method: request.method,
		path,
		target,
		body: request.body,
		headers: request.headers,
		auth: crossInstance ? 'none' : request.auth,
		retries: request.retries,
		timeoutMs: request.timeoutMs,
		signal: request.signal,
		intercept: request.intercept,
		accountScopedWork,
	});
}

export function instanceRequest<T = unknown>(request: InstanceHTTPRequest): Promise<RestResponse<T>> {
	return sendInstanceRequest<T>(request, {kind: 'standard'});
}

export function instanceRequestWithinAccountTransition<T = unknown>(
	request: InstanceHTTPRequest,
	suspension: AccountScopedWorkSuspension,
): Promise<RestResponse<T>> {
	suspension.assertActive(suspension.reason);
	return sendInstanceRequest<T>(request, {kind: 'account-transition', suspension});
}
