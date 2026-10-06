// SPDX-License-Identifier: AGPL-3.0-or-later

import {GatewayRpcMethodError, GatewayRpcMethodErrorCodes} from '@app/api/infrastructure/GatewayRpcError';
import type {IGatewayRpcTransport} from '@app/api/infrastructure/IGatewayRpcTransport';
import {Logger} from '@app/api/Logger';
import {type Msg, RequestError, TimeoutError} from '@nats-io/transport-node';
import type {INatsConnectionManager} from '@pkgs/nats/src/INatsConnectionManager';

const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();

const NATS_REQUEST_TIMEOUT_MS = 5000;
const NATS_SUBJECT_PREFIX = 'rpc.gateway.';

interface NatsRpcResponse {
	ok: boolean;
	result?: unknown;
	error?: string;
}

function mapNatsRpcTransportError(error: unknown): GatewayRpcMethodError | null {
	if (error instanceof RequestError && error.isNoResponders()) {
		return new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.NO_RESPONDERS);
	}
	if (error instanceof TimeoutError) {
		return new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.TIMEOUT);
	}
	return null;
}

function decodeNatsRpcResponse(responseText: string): NatsRpcResponse {
	let parsed: unknown;
	try {
		parsed = JSON.parse(responseText);
	} catch {
		throw new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.INTERNAL_ERROR);
	}
	if (typeof parsed !== 'object' || parsed === null || !('ok' in parsed)) {
		throw new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.INTERNAL_ERROR);
	}
	if (typeof parsed.ok !== 'boolean') {
		throw new GatewayRpcMethodError(GatewayRpcMethodErrorCodes.INTERNAL_ERROR);
	}
	const result = 'result' in parsed ? parsed.result : undefined;
	const error = 'error' in parsed && typeof parsed.error === 'string' ? parsed.error : undefined;
	return {
		ok: parsed.ok,
		result,
		error,
	};
}

export class NatsGatewayRpcTransport implements IGatewayRpcTransport {
	private readonly connectionManager: INatsConnectionManager;

	constructor(connectionManager: INatsConnectionManager) {
		this.connectionManager = connectionManager;
	}

	async call(method: string, params: Record<string, unknown>): Promise<unknown> {
		const subject = `${NATS_SUBJECT_PREFIX}${method}`;
		const payload = textEncoder.encode(JSON.stringify(params));
		let responseMsg: Msg;
		try {
			if (this.connectionManager.isClosed()) {
				await this.connectionManager.connect();
			}
			const connection = this.connectionManager.getConnection();
			responseMsg = await connection.request(subject, payload, {timeout: NATS_REQUEST_TIMEOUT_MS});
		} catch (error) {
			const mappedError = mapNatsRpcTransportError(error);
			if (mappedError !== null) {
				if (mappedError.code === GatewayRpcMethodErrorCodes.NO_RESPONDERS) {
					Logger.warn({subject}, '[nats-rpc] no responders for subject');
				}
				throw mappedError;
			}
			throw error;
		}
		const responseText = textDecoder.decode(responseMsg.data);
		const response = decodeNatsRpcResponse(responseText);
		if (!response.ok) {
			throw new GatewayRpcMethodError(response.error ?? GatewayRpcMethodErrorCodes.INTERNAL_ERROR);
		}
		return response.result;
	}

	async destroy(): Promise<void> {
		await this.connectionManager.drain();
	}
}
