// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {DataPacket, DataPacket_Kind, RpcRequest} from '@livekit/protocol';
import {EventEmitter} from 'events';
import type TypedEmitter from 'typed-emitter';
import type {StructuredLogger} from '../../../logger.ts';
import {CLIENT_PROTOCOL_DATA_STREAM_RPC} from '../../../version.ts';
import type {TextStreamReader} from '../../data-stream/incoming/StreamReader.ts';
import type OutgoingDataStreamManager from '../../data-stream/outgoing/OutgoingDataStreamManager.ts';
import type Participant from '../../participant/Participant.ts';
import {compareVersions, Future} from '../../utils.ts';
import {
	byteLength,
	MAX_V1_PAYLOAD_BYTES,
	type PerformRpcParams,
	RPC_REQUEST_DATA_STREAM_TOPIC,
	RPC_VERSION_V1,
	RPC_VERSION_V2,
	RpcError,
	RpcRequestAttrs,
} from '../utils.ts';
import type {RpcClientManagerCallbacks} from './events.ts';

export default class RpcClientManager extends (EventEmitter as new () => TypedEmitter<RpcClientManagerCallbacks>) {
	private log: StructuredLogger;

	private outgoingDataStreamManager: OutgoingDataStreamManager;

	private getRemoteParticipantClientProtocol: (identity: Participant['identity']) => number;

	private getServerVersion: () => string | undefined;

	private pendingAcks = new Map<string, {resolve: () => void; participantIdentity: string}>();

	private pendingResponses = new Map<
		string,
		{
			completionFuture: Future<string, RpcError>;
			participantIdentity: string;
		}
	>();

	constructor(
		log: StructuredLogger,
		outgoingDataStreamManager: OutgoingDataStreamManager,
		getRemoteParticipantClientProtocol: (identity: Participant['identity']) => number,
		getServerVersion: () => string | undefined,
	) {
		super();
		this.log = log;
		this.outgoingDataStreamManager = outgoingDataStreamManager;
		this.getRemoteParticipantClientProtocol = getRemoteParticipantClientProtocol;
		this.getServerVersion = getServerVersion;
	}

	async performRpc({
		destinationIdentity,
		method,
		payload,
		responseTimeout: responseTimeoutMs = 15000,
	}: PerformRpcParams): Promise<[id: string, completionPromise: Promise<string>]> {
		const maxRoundTripLatencyMs = 7000;
		const minEffectiveTimeoutMs = maxRoundTripLatencyMs + 1000;

		const remoteClientProtocol = this.getRemoteParticipantClientProtocol(destinationIdentity);
		const payloadBytes = byteLength(payload);

		if (payloadBytes > MAX_V1_PAYLOAD_BYTES && remoteClientProtocol < CLIENT_PROTOCOL_DATA_STREAM_RPC) {
			throw RpcError.builtIn('REQUEST_PAYLOAD_TOO_LARGE');
		}

		const serverVersion = this.getServerVersion();
		if (serverVersion && compareVersions(serverVersion, '1.8.0') < 0) {
			throw RpcError.builtIn('UNSUPPORTED_SERVER');
		}

		const effectiveTimeoutMs = Math.max(responseTimeoutMs, minEffectiveTimeoutMs);
		const id = crypto.randomUUID();

		const completionFuture = new Future<string, RpcError>();

		let responseTimeoutId: ReturnType<typeof setTimeout> | null = null;
		const ackTimeoutId = setTimeout(() => {
			this.pendingAcks.delete(id);
			completionFuture.reject?.(RpcError.builtIn('CONNECTION_TIMEOUT'));
			this.pendingResponses.delete(id);
			if (responseTimeoutId !== null) {
				clearTimeout(responseTimeoutId);
			}
		}, maxRoundTripLatencyMs);

		this.pendingAcks.set(id, {
			resolve: () => {
				clearTimeout(ackTimeoutId);
			},
			participantIdentity: destinationIdentity,
		});

		this.pendingResponses.set(id, {
			completionFuture,
			participantIdentity: destinationIdentity,
		});

		await this.publishRpcRequest(destinationIdentity, id, method, payload, effectiveTimeoutMs, remoteClientProtocol);

		responseTimeoutId = setTimeout(() => {
			this.pendingResponses.delete(id);
			completionFuture.reject?.(RpcError.builtIn('RESPONSE_TIMEOUT'));
		}, responseTimeoutMs);

		const completionPromise = completionFuture.promise.finally(() => {
			clearTimeout(responseTimeoutId);

			if (this.pendingAcks.has(id)) {
				this.log.warn('RPC response received before ack', id);
				this.pendingAcks.delete(id);
				clearTimeout(ackTimeoutId);
			}
		});

		return [id, completionPromise];
	}

	private async publishRpcRequest(
		destinationIdentity: string,
		requestId: string,
		method: string,
		payload: string,
		responseTimeout: number,
		remoteClientProtocol: number,
	) {
		if (remoteClientProtocol >= CLIENT_PROTOCOL_DATA_STREAM_RPC) {
			await this.outgoingDataStreamManager.sendText(payload, {
				topic: RPC_REQUEST_DATA_STREAM_TOPIC,
				destinationIdentities: [destinationIdentity],
				attributes: {
					[RpcRequestAttrs.RPC_REQUEST_ID]: requestId,
					[RpcRequestAttrs.RPC_REQUEST_METHOD]: method,
					[RpcRequestAttrs.RPC_REQUEST_RESPONSE_TIMEOUT_MS]: `${responseTimeout}`,
					[RpcRequestAttrs.RPC_REQUEST_VERSION]: `${RPC_VERSION_V2}`,
				},
			});
			return;
		}

		this.emit('sendDataPacket', {
			packet: new DataPacket({
				destinationIdentities: [destinationIdentity],
				kind: DataPacket_Kind.RELIABLE,
				value: {
					case: 'rpcRequest',
					value: new RpcRequest({
						id: requestId,
						method,
						payload,
						responseTimeoutMs: responseTimeout,
						version: RPC_VERSION_V1,
					}),
				},
			}),
		});
	}

	async handleIncomingDataStream(
		reader: TextStreamReader,
		senderIdentity: Participant['identity'],
		attributes: Record<string, string>,
	) {
		const associatedRequestId = attributes[RpcRequestAttrs.RPC_REQUEST_ID];
		if (!associatedRequestId) {
			this.log.warn(`RPC data stream malformed: ${RpcRequestAttrs.RPC_REQUEST_ID} not set.`);
			return;
		}

		const pending = this.pendingResponses.get(associatedRequestId);
		if (pending && pending.participantIdentity !== senderIdentity) {
			this.log.warn(
				`RPC response stream for ${associatedRequestId} arrived from unexpected sender ${senderIdentity}, expected ${pending.participantIdentity}. Ignoring.`,
			);
			return;
		}

		let payload: string;
		try {
			payload = await reader.readAll();
		} catch (e) {
			this.log.warn(`Error reading RPC response payload: ${e}`);
			this.handleIncomingRpcResponseFailure(
				associatedRequestId,
				RpcError.builtIn('APPLICATION_ERROR', 'Error reading RPC response payload', {cause: e}),
			);
			return;
		}

		this.handleIncomingRpcResponseSuccess(associatedRequestId, payload);
	}

	handleIncomingRpcResponseSuccess(requestId: string, payload: string) {
		const handler = this.pendingResponses.get(requestId);
		if (handler) {
			handler.completionFuture.resolve?.(payload);
			this.pendingResponses.delete(requestId);
		} else {
			this.log.error('Response received for unexpected RPC request', requestId);
		}
	}

	handleIncomingRpcResponseFailure(requestId: string, error: RpcError) {
		const handler = this.pendingResponses.get(requestId);
		if (handler) {
			handler.completionFuture.reject?.(error);
			this.pendingResponses.delete(requestId);
		} else {
			this.log.error('Response received for unexpected RPC request', requestId);
		}
	}

	handleIncomingRpcAck(requestId: string) {
		const handler = this.pendingAcks.get(requestId);
		if (handler) {
			handler.resolve();
			this.pendingAcks.delete(requestId);
		} else {
			this.log.error(`Ack received for unexpected RPC request: ${requestId}`);
		}
	}

	handleParticipantDisconnected(participantIdentity: string) {
		for (const [id, {participantIdentity: pendingIdentity}] of this.pendingAcks) {
			if (pendingIdentity === participantIdentity) {
				this.pendingAcks.delete(id);
			}
		}

		for (const [id, {participantIdentity: pendingIdentity, completionFuture}] of this.pendingResponses) {
			if (pendingIdentity === participantIdentity) {
				completionFuture.reject?.(RpcError.builtIn('RECIPIENT_DISCONNECTED'));
				this.pendingResponses.delete(id);
			}
		}
	}
}
