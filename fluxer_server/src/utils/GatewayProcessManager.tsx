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

import {type ChildProcess, spawn, spawnSync} from 'node:child_process';
import {randomBytes} from 'node:crypto';
import {existsSync} from 'node:fs';
import {Config} from '@app/Config';
import {Logger} from '@app/Logger';

const GATEWAY_STARTUP_DELAY_MS = 1000;
const GATEWAY_SHUTDOWN_TIMEOUT_MS = 2000;
const GATEWAY_PATH = '/opt/fluxer_gateway/bin/fluxer_gateway';
const GATEWAY_DIST_PORT = 9100;

export interface GatewayProcessManager {
	start: () => Promise<void>;
	stop: () => Promise<void>;
	isRunning: () => boolean;
}

function generateUniqueNodeName(): string {
	const uniqueId = randomBytes(4).toString('hex');
	const timestamp = Date.now();
	return `fluxer_gateway_${timestamp}_${uniqueId}@127.0.0.1`;
}

function killStaleGatewayProcesses(logger: Logger): void {
	const result = spawnSync('pgrep', ['-f', 'fluxer_gateway.*@'], {timeout: 1000, encoding: 'utf-8'});
	if (result.status === 0 && result.stdout) {
		const pids = result.stdout.trim().split('\n').filter(Boolean);
		for (const pid of pids) {
			logger.info({pid}, 'Killing stale gateway BEAM process');
			spawnSync('kill', ['-9', pid], {timeout: 1000});
		}
	}
}

function killEpmd(logger: Logger): void {
	const result = spawnSync('pgrep', ['-f', 'epmd'], {timeout: 1000, encoding: 'utf-8'});
	if (result.status === 0 && result.stdout) {
		const pids = result.stdout.trim().split('\n').filter(Boolean);
		for (const pid of pids) {
			logger.info({pid}, 'Killing EPMD process');
			spawnSync('kill', ['-9', pid], {timeout: 1000});
		}
	}
}

function freeDistributionPort(logger: Logger): void {
	const result = spawnSync('lsof', ['-ti', `:${GATEWAY_DIST_PORT}`], {timeout: 1000, encoding: 'utf-8'});
	if (result.status === 0 && result.stdout) {
		const pids = result.stdout.trim().split('\n').filter(Boolean);
		for (const pid of pids) {
			logger.info({pid, port: GATEWAY_DIST_PORT}, 'Killing process holding distribution port');
			spawnSync('kill', ['-9', pid], {timeout: 1000});
		}
	}
}

export function createGatewayProcessManager(): GatewayProcessManager {
	let gatewayProcess: ChildProcess | null = null;
	let startupFailed = false;
	let currentNodeName: string | null = null;
	const logger = Logger.child({component: 'gateway-process'});

	function forwardStream(stream: NodeJS.ReadableStream, level: 'info' | 'error'): void {
		stream.on('data', (data) => {
			const msg = data.toString().trim();
			if (msg) {
				if (msg.includes('GLIBC') && msg.includes('not found')) {
					logger.warn(
						{source: 'gateway'},
						'Gateway binary requires newer glibc. WebSocket gateway unavailable. Update your Docker base image or rebuild the gateway.',
					);
					startupFailed = true;
				} else {
					logger[level]({source: 'gateway'}, msg);
				}
			}
		});
	}

	async function waitForStartup(): Promise<void> {
		await new Promise((resolve) => setTimeout(resolve, GATEWAY_STARTUP_DELAY_MS));
	}

	async function gracefulShutdown(process: ChildProcess): Promise<void> {
		logger.info('Sending SIGTERM to gateway process');
		process.kill('SIGTERM');
		await new Promise((resolve) => setTimeout(resolve, GATEWAY_SHUTDOWN_TIMEOUT_MS));
		if (gatewayProcess) {
			logger.warn('Gateway did not shutdown gracefully, force killing');
			process.kill('SIGKILL');
		}
	}

	const start = async (): Promise<void> => {
		if (!existsSync(GATEWAY_PATH)) {
			logger.warn('Gateway binary not found, WebSocket gateway unavailable');
			startupFailed = true;
			return;
		}

		killStaleGatewayProcesses(logger);
		killEpmd(logger);
		freeDistributionPort(logger);

		await new Promise((resolve) => setTimeout(resolve, 100));

		currentNodeName = generateUniqueNodeName();
		logger.info({nodeName: currentNodeName}, 'Starting Fluxer Gateway process with unique node name');

		const gatewayEnv = {
			...process.env,
			FLUXER_GATEWAY_HOST: Config.services.gateway.port ? '0.0.0.0' : '127.0.0.1',
			FLUXER_GATEWAY_PORT: Config.services.gateway.port.toString(),
			FLUXER_GATEWAY_NODE_FLAG: '-name',
			FLUXER_GATEWAY_NODE_NAME: currentNodeName,
			ERL_DIST_PORT: GATEWAY_DIST_PORT.toString(),
		};

		try {
			gatewayProcess = spawn(GATEWAY_PATH, ['foreground'], {
				env: gatewayEnv,
				stdio: ['ignore', 'pipe', 'pipe'],
			});

			if (gatewayProcess.stdout) {
				forwardStream(gatewayProcess.stdout, 'info');
			}

			if (gatewayProcess.stderr) {
				forwardStream(gatewayProcess.stderr, 'error');
			}

			gatewayProcess.on('error', (err) => {
				logger.warn({error: err.message}, 'Gateway process error, WebSocket gateway unavailable');
				startupFailed = true;
			});

			gatewayProcess.on('exit', (code, signal) => {
				if (code !== 0 && code !== null) {
					if (!startupFailed) {
						logger.warn({code, signal}, 'Gateway process exited unexpectedly, WebSocket gateway unavailable');
					}
					startupFailed = true;
				} else {
					logger.info({code, signal}, 'Gateway process exited');
				}
				gatewayProcess = null;
				currentNodeName = null;
			});

			await waitForStartup();

			if (gatewayProcess && !startupFailed) {
				logger.info('Gateway process started successfully');
			}
		} catch (error) {
			logger.warn(
				{error: error instanceof Error ? error.message : 'Unknown error'},
				'Failed to spawn gateway process, WebSocket gateway unavailable',
			);
			startupFailed = true;
		}
	};

	const stop = async (): Promise<void> => {
		if (gatewayProcess) {
			logger.info('Stopping Gateway process');
			await gracefulShutdown(gatewayProcess);
		}
	};

	const isRunning = (): boolean => {
		return gatewayProcess !== null && !startupFailed;
	};

	return {
		start,
		stop,
		isRunning,
	};
}
