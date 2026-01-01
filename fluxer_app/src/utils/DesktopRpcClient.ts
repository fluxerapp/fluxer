/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import Config from '~/Config';

const RPC_PORT_STABLE = 21863;
const RPC_PORT_CANARY = 21864;

const RPC_PORTS =
	Config.PUBLIC_PROJECT_ENV === 'canary' ? [RPC_PORT_CANARY, RPC_PORT_STABLE] : [RPC_PORT_STABLE, RPC_PORT_CANARY];

type RpcResponse<T = unknown> = {
	success: boolean;
	data?: T;
	error?: string;
};

interface HealthResponse {
	status: string;
	channel: string;
	version: string;
	platform: string;
}

interface NavigateResponse {
	navigated: boolean;
	path: string;
}

let cachedAvailablePort: number | null = null;
let lastHealthCheck = 0;
const HEALTH_CHECK_CACHE_MS = 5000;

const rpcFetch = async <T>(port: number, endpoint: string, options?: RequestInit): Promise<RpcResponse<T> | null> => {
	const controller = new AbortController();
	const timeout = setTimeout(() => controller.abort(), 2000);

	try {
		const response = await fetch(`http://127.0.0.1:${port}${endpoint}`, {
			...options,
			signal: controller.signal,
		});
		return (await response.json()) as RpcResponse<T>;
	} catch {
		return null;
	} finally {
		clearTimeout(timeout);
	}
};

export const checkDesktopAvailable = async (): Promise<{
	available: boolean;
	port: number | null;
	info: HealthResponse | null;
}> => {
	const now = Date.now();

	if (cachedAvailablePort !== null && now - lastHealthCheck < HEALTH_CHECK_CACHE_MS) {
		const result = await rpcFetch<HealthResponse>(cachedAvailablePort, '/health');
		if (result?.success && result.data) {
			return {available: true, port: cachedAvailablePort, info: result.data};
		}
		cachedAvailablePort = null;
	}

	for (const port of RPC_PORTS) {
		const result = await rpcFetch<HealthResponse>(port, '/health');
		if (result?.success && result.data) {
			cachedAvailablePort = port;
			lastHealthCheck = now;
			return {available: true, port, info: result.data};
		}
	}

	cachedAvailablePort = null;
	return {available: false, port: null, info: null};
};

export const navigateInDesktop = async (path: string): Promise<{success: boolean; error?: string}> => {
	const {available, port} = await checkDesktopAvailable();

	if (!available || port === null) {
		return {success: false, error: 'Desktop app not available'};
	}

	const result = await rpcFetch<NavigateResponse>(port, '/navigate', {
		method: 'POST',
		headers: {'Content-Type': 'application/json'},
		body: JSON.stringify({method: 'navigate', params: {path}}),
	});

	if (!result) {
		return {success: false, error: 'Failed to communicate with desktop app'};
	}

	if (!result.success) {
		return {success: false, error: result.error ?? 'Unknown error'};
	}

	return {success: true};
};

export const focusDesktop = async (): Promise<{success: boolean; error?: string}> => {
	const {available, port} = await checkDesktopAvailable();

	if (!available || port === null) {
		return {success: false, error: 'Desktop app not available'};
	}

	const result = await rpcFetch(port, '/focus', {method: 'POST'});

	if (!result) {
		return {success: false, error: 'Failed to communicate with desktop app'};
	}

	if (!result.success) {
		return {success: false, error: result.error ?? 'Unknown error'};
	}

	return {success: true};
};

export const resetDesktopRpcCache = (): void => {
	cachedAvailablePort = null;
	lastHealthCheck = 0;
};
