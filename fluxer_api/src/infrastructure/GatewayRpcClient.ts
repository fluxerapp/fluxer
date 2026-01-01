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

import {Config} from '~/Config';
import {Logger} from '~/Logger';
import type {CallData} from './IGatewayService';

interface GatewayRpcResponse {
	result?: unknown;
	error?: unknown;
}

const MAX_RETRY_ATTEMPTS = 3;
const MAX_BACKOFF_MS = 5_000;
const INITIAL_BACKOFF_MS = 500;

export class GatewayRpcClient {
	private static instance: GatewayRpcClient | null = null;

	private readonly endpoint: string;

	private constructor() {
		this.endpoint = `http://${Config.gateway.rpcHost}:${Config.gateway.rpcPort}/_rpc`;
	}

	static getInstance(): GatewayRpcClient {
		if (!GatewayRpcClient.instance) {
			GatewayRpcClient.instance = new GatewayRpcClient();
		}
		return GatewayRpcClient.instance;
	}

	async call<T>(method: string, params: Record<string, unknown>): Promise<T> {
		Logger.debug(`[gateway-rpc] calling ${method}`);

		for (let attempt = 0; attempt <= MAX_RETRY_ATTEMPTS; attempt += 1) {
			try {
				return await this.executeCall(method, params);
			} catch (error) {
				if (attempt === MAX_RETRY_ATTEMPTS) {
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
		let response: Response;
		try {
			response = await fetch(this.endpoint, {
				method: 'POST',
				headers: {
					'Content-Type': 'application/json',
					Authorization: `Bearer ${Config.gateway.rpcSecret}`,
				},
				body: JSON.stringify({
					method,
					params,
				}),
			});
		} catch (error) {
			Logger.error({error}, '[gateway-rpc] request failed to reach gateway');
			throw error;
		}

		const text = await response.text();

		let payload: GatewayRpcResponse = {};
		if (text.length > 0) {
			try {
				payload = JSON.parse(text) as GatewayRpcResponse;
			} catch (error) {
				Logger.error({error, body: text, status: response.status}, '[gateway-rpc] failed to parse response body');
				throw new Error('Malformed gateway RPC response');
			}
		}

		if (!response.ok) {
			const message =
				typeof payload.error === 'string' ? payload.error : `Gateway RPC request failed with status ${response.status}`;
			throw new Error(message);
		}

		if (!Object.hasOwn(payload, 'result')) {
			Logger.error({status: response.status, body: payload}, '[gateway-rpc] response missing result value');
			throw new Error('Malformed gateway RPC response');
		}

		return payload.result as T;
	}

	private calculateBackoff(attempt: number): number {
		const multiplier = 2 ** attempt;
		return Math.min(INITIAL_BACKOFF_MS * multiplier, MAX_BACKOFF_MS);
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
	): Promise<CallData> {
		return this.call<CallData>('call.create', {
			channel_id: channelId,
			message_id: messageId,
			region,
			ringing,
			recipients,
		});
	}

	async updateCallRegion(channelId: string, region: string): Promise<boolean> {
		return this.call('call.update_region', {channel_id: channelId, region});
	}

	async ringCallRecipients(channelId: string, recipients: Array<string>): Promise<boolean> {
		return this.call('call.ring', {channel_id: channelId, recipients});
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
