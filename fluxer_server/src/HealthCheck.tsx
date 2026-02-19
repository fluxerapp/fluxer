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

import type {InitializedServices} from '@app/ServiceInitializer';
import type {Context} from 'hono';

export type ServiceStatus = 'healthy' | 'degraded' | 'unhealthy' | 'disabled';
export interface ServiceHealth {
	status: ServiceStatus;
	message?: string;
	latencyMs?: number;
	details?: Record<string, unknown>;
}

export interface HealthCheckResponse {
	status: ServiceStatus;
	timestamp: string;
	uptime: number;
	version: string;
	services: {
		kv: ServiceHealth;
		s3: ServiceHealth;
		jetstream: ServiceHealth;
		mediaProxy: ServiceHealth;
		admin: ServiceHealth;
		api: ServiceHealth;
		app: ServiceHealth;
	};
}

export interface HealthCheckConfig {
	services: InitializedServices;
	staticDir?: string;
	version: string;
	startTime: number;
	latencyThresholdMs: number;
}

async function checkKVHealth(services: InitializedServices, latencyThresholdMs: number): Promise<ServiceHealth> {
	if (services.kv === undefined) {
		return {status: 'disabled'};
	}

	try {
		const start = Date.now();
		const healthy = await services.kv.health();
		const latencyMs = Date.now() - start;

		if (!healthy) {
			return {
				status: 'unhealthy',
				latencyMs,
				message: 'KV provider health check failed',
			};
		}

		if (latencyMs > latencyThresholdMs) {
			return {
				status: 'degraded',
				latencyMs,
				message: 'High latency detected',
			};
		}

		return {
			status: 'healthy',
			latencyMs,
		};
	} catch (error) {
		return {
			status: 'unhealthy',
			message: error instanceof Error ? error.message : 'Unknown error',
		};
	}
}

async function checkS3Health(services: InitializedServices, latencyThresholdMs: number): Promise<ServiceHealth> {
	if (services.s3 === undefined) {
		return {status: 'disabled'};
	}

	try {
		const start = Date.now();
		const s3Service = services.s3.getS3Service();
		const buckets = await s3Service.listBuckets();
		const latencyMs = Date.now() - start;

		if (latencyMs > latencyThresholdMs) {
			return {
				status: 'degraded',
				latencyMs,
				message: 'High latency detected',
				details: {bucketCount: buckets.length},
			};
		}

		return {
			status: 'healthy',
			latencyMs,
			details: {bucketCount: buckets.length},
		};
	} catch (error) {
		return {
			status: 'unhealthy',
			message: error instanceof Error ? error.message : 'Unknown error',
		};
	}
}

function checkJetStreamHealth(services: InitializedServices): ServiceHealth {
	if (services.jsConnectionManager === undefined) {
		return {status: 'disabled'};
	}

	if (services.jsConnectionManager.isClosed()) {
		return {
			status: 'unhealthy',
			message: 'JetStream connection is closed',
		};
	}

	return {status: 'healthy'};
}

async function checkMediaProxyHealth(services: InitializedServices): Promise<ServiceHealth> {
	if (services.mediaProxy === undefined) {
		return {status: 'disabled'};
	}

	return {
		status: 'healthy',
	};
}

async function checkAdminHealth(services: InitializedServices): Promise<ServiceHealth> {
	if (services.admin === undefined) {
		return {status: 'disabled'};
	}

	return {
		status: 'healthy',
	};
}

async function checkAPIHealth(services: InitializedServices): Promise<ServiceHealth> {
	if (services.api === undefined) {
		return {status: 'disabled'};
	}

	return {
		status: 'healthy',
	};
}

async function checkAppServerHealth(services: InitializedServices, staticDir?: string): Promise<ServiceHealth> {
	if (services.appServer === undefined) {
		if (staticDir === undefined) {
			return {
				status: 'disabled',
				message: 'No static directory configured',
			};
		}
		return {status: 'disabled'};
	}

	return {
		status: 'healthy',
	};
}

function determineOverallStatus(services: HealthCheckResponse['services']): ServiceStatus {
	const statuses = Object.values(services).map((h) => h.status);

	if (statuses.some((s) => s === 'unhealthy')) {
		return 'unhealthy';
	}

	if (statuses.some((s) => s === 'degraded')) {
		return 'degraded';
	}

	return 'healthy';
}

export function createHealthCheckHandler(config: HealthCheckConfig) {
	return async (c: Context): Promise<Response> => {
		const {services, staticDir, version, startTime, latencyThresholdMs} = config;

		const healthChecks: HealthCheckResponse['services'] = {
			kv: await checkKVHealth(services, latencyThresholdMs),
			s3: await checkS3Health(services, latencyThresholdMs),
			jetstream: checkJetStreamHealth(services),
			mediaProxy: await checkMediaProxyHealth(services),
			admin: await checkAdminHealth(services),
			api: await checkAPIHealth(services),
			app: await checkAppServerHealth(services, staticDir),
		};

		const overallStatus = determineOverallStatus(healthChecks);

		const response: HealthCheckResponse = {
			status: overallStatus,
			timestamp: new Date().toISOString(),
			uptime: Math.floor((Date.now() - startTime) / 1000),
			version,
			services: healthChecks,
		};

		const statusCode = overallStatus === 'unhealthy' ? 503 : 200;
		return c.json(response, statusCode);
	};
}

export interface ReadinessCheckResponse {
	ready: boolean;
	timestamp: string;
	checks: {
		database?: {ready: boolean; message?: string};
		kv?: {ready: boolean; message?: string};
		s3?: {ready: boolean; message?: string};
		jetstream?: {ready: boolean; message?: string};
	};
}

export function createReadinessCheckHandler(config: HealthCheckConfig) {
	return async (c: Context): Promise<Response> => {
		const {services} = config;

		const checks: ReadinessCheckResponse['checks'] = {};
		let allReady = true;

		if (services.kv !== undefined) {
			try {
				const healthy = await services.kv.health();
				checks.kv = healthy ? {ready: true} : {ready: false, message: 'KV provider health check failed'};
				if (!healthy) {
					allReady = false;
				}
			} catch (error) {
				checks.kv = {
					ready: false,
					message: error instanceof Error ? error.message : 'Unknown error',
				};
				allReady = false;
			}
		}

		if (services.s3 !== undefined) {
			try {
				const s3Service = services.s3.getS3Service();
				await s3Service.listBuckets();
				checks.s3 = {ready: true};
			} catch (error) {
				checks.s3 = {
					ready: false,
					message: error instanceof Error ? error.message : 'Unknown error',
				};
				allReady = false;
			}
		}

		if (services.jsConnectionManager !== undefined) {
			if (services.jsConnectionManager.isClosed()) {
				checks.jetstream = {ready: false, message: 'JetStream connection is closed'};
				allReady = false;
			} else {
				checks.jetstream = {ready: true};
			}
		}

		const response: ReadinessCheckResponse = {
			ready: allReady,
			timestamp: new Date().toISOString(),
			checks,
		};

		const statusCode = allReady ? 200 : 503;
		return c.json(response, statusCode);
	};
}

export interface LivenessCheckResponse {
	alive: boolean;
	timestamp: string;
}

export function createLivenessCheckHandler() {
	return async (c: Context): Promise<Response> => {
		const response: LivenessCheckResponse = {
			alive: true,
			timestamp: new Date().toISOString(),
		};

		return c.json(response, 200);
	};
}
