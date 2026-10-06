// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {DataPacket, DataPacket_Kind, RpcAck, type RpcRequest, RpcResponse} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEmitter from 'typed-emitter';
import type {StructuredLogger} from '../../../logger.ts';
import {CLIENT_PROTOCOL_DATA_STREAM_RPC} from '../../../version.ts';
import type {TextStreamReader} from '../../data-stream/incoming/StreamReader.ts';
import type OutgoingDataStreamManager from '../../data-stream/outgoing/OutgoingDataStreamManager.ts';
import type Participant from '../../participant/Participant.ts';
import {
	byteLength,
	MAX_V1_PAYLOAD_BYTES,
	RPC_RESPONSE_DATA_STREAM_TOPIC,
	RPC_VERSION_V2,
	RpcError,
	type RpcInvocationData,
	RpcRequestAttrs,
} from '../utils.ts';
import type {RpcServerManagerCallbacks} from './events.ts';

export default class RpcServerManager extends (EventEmitter as new () => TypedEmitter<RpcServerManagerCallbacks>) {
	private log: StructuredLogger;

	private outgoingDataStreamManager: OutgoingDataStreamManager;

	private getRemoteParticipantClientProtocol: (identity: Participant['identity']) => number;

	private rpcHandlers: Map<string, (data: RpcInvocationData) => Promise<string>> = new Map();

	constructor(
		log: StructuredLogger,
		outgoingDataStreamManager: OutgoingDataStreamManager,
		getRemoteParticipantClientProtocol: (identity: Participant['identity']) => number,
	) {
		super();
		this.log = log;
		this.outgoingDataStreamManager = outgoingDataStreamManager;
		this.getRemoteParticipantClientProtocol = getRemoteParticipantClientProtocol;
	}

	registerRpcMethod(method: string, handler: (data: RpcInvocationData) => Promise<string>) {
		if (this.rpcHandlers.has(method)) {
			throw Error(
				`RPC handler already registered for method ${method}, unregisterRpcMethod before trying to register again`,
			);
		}
		this.rpcHandlers.set(method, handler);
	}

	unregisterRpcMethod(method: string) {
		this.rpcHandlers.delete(method);
	}

	async handleIncomingRpcRequest(callerIdentity: string, rpcRequest: RpcRequest) {
		this.publishRpcAck(callerIdentity, rpcRequest.id);

		if (rpcRequest.version !== 1) {
			this.publishRpcResponsePacket(callerIdentity, rpcRequest.id, null, RpcError.builtIn('UNSUPPORTED_VERSION'));
			return;
		}

		const handler = this.rpcHandlers.get(rpcRequest.method);

		if (!handler) {
			this.publishRpcResponsePacket(callerIdentity, rpcRequest.id, null, RpcError.builtIn('UNSUPPORTED_METHOD'));
			return;
		}

		let response: Awaited<ReturnType<typeof handler>>;
		try {
			response = await handler({
				requestId: rpcRequest.id,
				callerIdentity,
				payload: rpcRequest.payload,
				responseTimeout: rpcRequest.responseTimeoutMs,
			});
		} catch (error) {
			let responseError: RpcError;
			if (error instanceof RpcError) {
				responseError = error;
			} else {
				this.log.warn(
					`Uncaught error returned by RPC handler for ${rpcRequest.method}. Returning APPLICATION_ERROR instead.`,
					error,
				);
				responseError = RpcError.builtIn('APPLICATION_ERROR', `Uncaught error: ${(error as Error)?.message ?? error}`, {
					cause: error,
				});
			}

			this.publishRpcResponsePacket(callerIdentity, rpcRequest.id, null, responseError);
			return;
		}

		await this.publishRpcResponse(callerIdentity, rpcRequest.id, response ?? '');
	}

	async handleIncomingDataStream(
		reader: TextStreamReader,
		callerIdentity: Participant['identity'],
		dataStreamAttrs: Record<string, string>,
	) {
		const requestId = dataStreamAttrs[RpcRequestAttrs.RPC_REQUEST_ID];
		const method = dataStreamAttrs[RpcRequestAttrs.RPC_REQUEST_METHOD];
		const responseTimeout = parseInt(dataStreamAttrs[RpcRequestAttrs.RPC_REQUEST_RESPONSE_TIMEOUT_MS], 10);
		const version = parseInt(dataStreamAttrs[RpcRequestAttrs.RPC_REQUEST_VERSION], 10);

		if (!requestId || !method || Number.isNaN(responseTimeout) || Number.isNaN(version)) {
			this.log.warn(
				`RPC data stream malformed: ${RpcRequestAttrs.RPC_REQUEST_ID} / ${RpcRequestAttrs.RPC_REQUEST_METHOD} / ${RpcRequestAttrs.RPC_REQUEST_RESPONSE_TIMEOUT_MS} / ${RpcRequestAttrs.RPC_REQUEST_VERSION} not set.`,
			);
			this.publishRpcResponsePacket(
				callerIdentity,
				requestId,
				null,
				RpcError.builtIn('APPLICATION_ERROR', 'RPC data stream malformed'),
			);
			return;
		}

		this.publishRpcAck(callerIdentity, requestId);

		if (version !== RPC_VERSION_V2) {
			this.publishRpcResponsePacket(callerIdentity, requestId, null, RpcError.builtIn('UNSUPPORTED_VERSION'));
			return;
		}

		let payload: string;
		try {
			payload = await reader.readAll();
		} catch (e) {
			this.log.warn(`Error reading RPC request payload: ${e}`);
			this.publishRpcResponsePacket(
				callerIdentity,
				requestId,
				null,
				RpcError.builtIn('APPLICATION_ERROR', 'Error reading RPC request payload', {cause: e}),
			);
			return;
		}

		const handler = this.rpcHandlers.get(method);

		if (!handler) {
			this.publishRpcResponsePacket(callerIdentity, requestId, null, RpcError.builtIn('UNSUPPORTED_METHOD'));
			return;
		}

		let response: Awaited<ReturnType<typeof handler>>;
		try {
			response = await handler({
				requestId,
				callerIdentity,
				payload,
				responseTimeout,
			});
		} catch (error) {
			let responseError: RpcError;
			if (error instanceof RpcError) {
				responseError = error;
			} else {
				this.log.warn(
					`Uncaught error returned by RPC handler for ${method}. Returning APPLICATION_ERROR instead.`,
					error,
				);
				responseError = RpcError.builtIn('APPLICATION_ERROR');
			}

			this.publishRpcResponsePacket(callerIdentity, requestId, null, responseError);
			return;
		}

		await this.publishRpcResponse(callerIdentity, requestId, response ?? '');
	}

	private publishRpcAck(destinationIdentity: string, requestId: string) {
		this.emit('sendDataPacket', {
			packet: new DataPacket({
				destinationIdentities: [destinationIdentity],
				kind: DataPacket_Kind.RELIABLE,
				value: {
					case: 'rpcAck',
					value: new RpcAck({
						requestId,
					}),
				},
			}),
		});
	}

	private publishRpcResponsePacket(
		destinationIdentity: string,
		requestId: string,
		payload: string | null,
		error: RpcError | null,
	) {
		this.emit('sendDataPacket', {
			packet: new DataPacket({
				destinationIdentities: [destinationIdentity],
				kind: DataPacket_Kind.RELIABLE,
				value: {
					case: 'rpcResponse',
					value: new RpcResponse({
						requestId,
						value: error ? {case: 'error', value: error.toProto()} : {case: 'payload', value: payload ?? ''},
					}),
				},
			}),
		});
	}

	private async publishRpcResponse(destinationIdentity: string, requestId: string, payload: string) {
		const callerClientProtocol = this.getRemoteParticipantClientProtocol(destinationIdentity);

		if (callerClientProtocol >= CLIENT_PROTOCOL_DATA_STREAM_RPC) {
			await this.outgoingDataStreamManager.sendText(payload, {
				topic: RPC_RESPONSE_DATA_STREAM_TOPIC,
				destinationIdentities: [destinationIdentity],
				attributes: {[RpcRequestAttrs.RPC_REQUEST_ID]: requestId},
			});
			return;
		}

		const responseBytes = byteLength(payload);
		if (responseBytes > MAX_V1_PAYLOAD_BYTES) {
			this.log.warn(
				`RPC Response payload too large for request ${requestId}. To send larger responses, consider updating the sending client.`,
			);
			this.publishRpcResponsePacket(
				destinationIdentity,
				requestId,
				null,
				RpcError.builtIn('RESPONSE_PAYLOAD_TOO_LARGE'),
			);
			return;
		}

		this.publishRpcResponsePacket(destinationIdentity, requestId, payload, null);
	}
}
