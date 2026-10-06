// SPDX-License-Identifier: AGPL-3.0-or-later

import {Config} from '@app/api/Config';
import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@app/api/infrastructure/GatewayRpcError';
import type {IGatewayRpcTransport} from '@app/api/infrastructure/IGatewayRpcTransport';
import {type CallCaller, type CallData, callCallerRpcParams} from '@app/api/infrastructure/IGatewayService';
import {NatsGatewayRpcTransport} from '@app/api/infrastructure/NatsGatewayRpcTransport';
import {Logger} from '@app/api/Logger';
import {NatsConnectionManager} from '@pkgs/nats/src/NatsConnectionManager';
import {ms} from 'itty-time';

const MAX_RETRY_ATTEMPTS = 2;

export class GatewayRpcClient {
	private static instance: GatewayRpcClient | null = null;
	private readonly transport: IGatewayRpcTransport;

	private constructor(transport: IGatewayRpcTransport) {
		this.transport = transport;
	}

	static getInstance(): GatewayRpcClient {
		if (!GatewayRpcClient.instance) {
			GatewayRpcClient.instance = new GatewayRpcClient(createNatsTransportSync());
		}
		return GatewayRpcClient.instance;
	}

	static createForTests(transport: IGatewayRpcTransport): GatewayRpcClient {
		const client = new GatewayRpcClient(transport);
		GatewayRpcClient.instance = client;
		return client;
	}

	static async resetForTests(): Promise<void> {
		if (!GatewayRpcClient.instance) {
			return;
		}
		await GatewayRpcClient.instance.transport.destroy();
		GatewayRpcClient.instance = null;
	}

	async call<T>(method: string, params: Record<string, unknown>): Promise<T> {
		Logger.debug(`[gateway-rpc] calling ${method}`);
		for (let attempt = 0; attempt <= MAX_RETRY_ATTEMPTS; attempt += 1) {
			try {
				const result = await this.executeCall(method, params);
				return result as T;
			} catch (error) {
				const shouldRetry = this.shouldRetry(error, method);
				if (attempt === MAX_RETRY_ATTEMPTS || !shouldRetry) {
					throw error;
				}
				const backoffMs = this.calculateBackoff(attempt);
				Logger.warn({error, attempt: attempt + 1, backoffMs}, '[gateway-rpc] retrying failed request');
				await this.delay(backoffMs);
			}
		}
		throw new Error('Unexpected gateway RPC retry failure');
	}

	private async executeCall<T>(method: string, params: Record<string, unknown>): Promise<T> {
		const result = await this.transport.call(method, params);
		return result as T;
	}

	private calculateBackoff(attempt: number): number {
		if (attempt === 0) {
			return 100;
		}
		const multiplier = 2 ** attempt;
		return Math.min(500 * multiplier, ms('2 seconds'));
	}

	private shouldRetry(error: unknown, method: string): boolean {
		if (this.isNatsConnectionError(error)) {
			return true;
		}
		if (!(error instanceof Error)) {
			return false;
		}
		return this.isRetryableOverloadError(error, method);
	}

	private isNatsConnectionError(error: unknown): boolean {
		if (!(error instanceof Error)) {
			return false;
		}
		if (error instanceof GatewayRpcMethodError) {
			return error.code === GatewayRpcMethodErrorCodes.NO_RESPONDERS;
		}
		const message = error.message.toLowerCase();
		return (
			message.includes('connection closed') ||
			message.includes('connection lost') ||
			message.includes('reconnect') ||
			message.includes('disconnect')
		);
	}

	private isRetryableOverloadError(error: Error, method: string): boolean {
		if (!this.isDispatchMethod(method)) {
			return false;
		}
		if (!(error instanceof GatewayRpcMethodError)) {
			return false;
		}
		return error.code === GatewayRpcMethodErrorCodes.OVERLOADED;
	}

	private isDispatchMethod(method: string): boolean {
		return method.endsWith('.dispatch');
	}

	private delay(ms: number): Promise<void> {
		return new Promise((resolve) => {
			setTimeout(resolve, ms);
		});
	}

	async getCall(channelId: string): Promise<CallData | null> {
		return this.call<CallData | null>('call.get', {channel_id: channelId});
	}

	async createCall(
		channelId: string,
		messageId: string,
		region: string,
		ringing: Array<string>,
		recipients: Array<string>,
		caller?: CallCaller,
	): Promise<CallData> {
		return this.call<CallData>('call.create', {
			channel_id: channelId,
			message_id: messageId,
			region,
			ringing,
			recipients,
			...callCallerRpcParams(caller),
		});
	}

	async updateCallRegion(channelId: string, region: string | null): Promise<boolean> {
		return this.call('call.update_region', {channel_id: channelId, region});
	}

	async ringCallRecipients(channelId: string, recipients: Array<string>, caller?: CallCaller): Promise<boolean> {
		return this.call('call.ring', {channel_id: channelId, recipients, ...callCallerRpcParams(caller)});
	}

	async stopRingingCallRecipients(channelId: string, recipients: Array<string>): Promise<boolean> {
		return this.call('call.stop_ringing', {channel_id: channelId, recipients});
	}

	async deleteCall(channelId: string): Promise<boolean> {
		return this.call('call.delete', {channel_id: channelId});
	}

	async getNodeStats(): Promise<unknown> {
		return this.call('process.node_stats', {});
	}
}

function createNatsTransportSync(): NatsGatewayRpcTransport {
	const manager = new NatsConnectionManager({
		url: Config.nats.coreUrl,
		token: Config.nats.authToken || undefined,
		name: 'fluxer-api-rpc',
	});
	void manager.connect().catch((error) => {
		Logger.error({error}, '[gateway-rpc] Failed to establish NATS connection');
	});
	return new NatsGatewayRpcTransport(manager);
}
