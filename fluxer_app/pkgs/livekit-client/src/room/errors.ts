// SPDX-FileCopyrightText: 2024 LiveKit, Inc.
//
// SPDX-License-Identifier: Apache-2.0
import {type DisconnectReason, RequestResponse_Reason} from '@livekit/protocol';

export class LivekitError extends Error {
	code: number;
	override cause?: unknown;

	constructor(code: number, message?: string, options?: {cause?: unknown}) {
		super(message || 'an error has occurred');
		this.name = 'LiveKitError';
		this.code = code;

		if (typeof options?.cause !== 'undefined') {
			this.cause = options?.cause;
		}
	}
}

export abstract class LivekitReasonedError<Reason> extends LivekitError {
	abstract reason: Reason;

	abstract reasonName: string;
}

export class SimulatedError extends LivekitError {
	override readonly name = 'simulated';

	constructor(message = 'Simulated failure') {
		super(-1, message);
	}
}

export enum ConnectionErrorReason {
	NotAllowed,
	ServerUnreachable,
	InternalError,
	Cancelled,
	LeaveRequest,
	Timeout,
	WebSocket,
	ServiceNotFound,
}

type NotAllowed = {
	reason: ConnectionErrorReason.NotAllowed;
	status: number;
	context?: unknown;
};

type InternalError = {
	reason: ConnectionErrorReason.InternalError;
	status: never;
	context?: {status?: number; statusText?: string};
};

type ConnectionTimeout = {
	reason: ConnectionErrorReason.Timeout;
	status: never;
	context: never;
};

type LeaveRequest = {
	reason: ConnectionErrorReason.LeaveRequest;
	status: never;
	context: DisconnectReason;
};

type Cancelled = {
	reason: ConnectionErrorReason.Cancelled;
	status: never;
	context: never;
};

type ServerUnreachable = {
	reason: ConnectionErrorReason.ServerUnreachable;
	status?: number;
	context?: never;
};

type WebSocket = {
	reason: ConnectionErrorReason.WebSocket;
	status?: number;
	context?: string;
};

type ServiceNotFound = {
	reason: ConnectionErrorReason.ServiceNotFound;
	status: never;
	context: string;
};

type ConnectionErrorVariants =
	| NotAllowed
	| ConnectionTimeout
	| LeaveRequest
	| InternalError
	| Cancelled
	| ServerUnreachable
	| WebSocket
	| ServiceNotFound;

export class ConnectionError<
	Variant extends ConnectionErrorVariants = ConnectionErrorVariants,
> extends LivekitReasonedError<Variant['reason']> {
	status?: Variant['status'];

	context: Variant['context'];

	reason: Variant['reason'];

	reasonName: string;
	override readonly name = 'ConnectionError';

	protected constructor(
		message: string,
		reason: Variant['reason'],
		status?: Variant['status'],
		context?: Variant['context'],
	) {
		super(1, message);
		this.status = status;
		this.reason = reason;
		this.context = context;
		this.reasonName = ConnectionErrorReason[reason];
	}

	static notAllowed(message: string, status: number, context?: unknown) {
		return new ConnectionError<NotAllowed>(message, ConnectionErrorReason.NotAllowed, status, context);
	}

	static timeout(message: string) {
		return new ConnectionError<ConnectionTimeout>(message, ConnectionErrorReason.Timeout);
	}

	static leaveRequest(message: string, context: DisconnectReason) {
		return new ConnectionError<LeaveRequest>(message, ConnectionErrorReason.LeaveRequest, undefined, context);
	}

	static internal(message: string, context?: {status?: number; statusText?: string}) {
		return new ConnectionError<InternalError>(message, ConnectionErrorReason.InternalError, undefined, context);
	}

	static cancelled(message: string) {
		return new ConnectionError<Cancelled>(message, ConnectionErrorReason.Cancelled);
	}

	static serverUnreachable(message: string, status?: number) {
		return new ConnectionError<ServerUnreachable>(message, ConnectionErrorReason.ServerUnreachable, status);
	}

	static websocket(message: string, status?: number, reason?: string) {
		return new ConnectionError<WebSocket>(message, ConnectionErrorReason.WebSocket, status, reason);
	}

	static serviceNotFound(message: string, serviceName: 'v0-rtc') {
		return new ConnectionError<ServiceNotFound>(message, ConnectionErrorReason.ServiceNotFound, undefined, serviceName);
	}
}

export class DeviceUnsupportedError extends LivekitError {
	override readonly name = 'DeviceUnsupportedError';

	constructor(message?: string) {
		super(21, message ?? 'device is unsupported');
	}
}

export class TrackInvalidError extends LivekitError {
	override readonly name = 'TrackInvalidError';

	constructor(message?: string) {
		super(20, message ?? 'track is invalid');
	}
}

export class UnsupportedServer extends LivekitError {
	override readonly name = 'UnsupportedServer';

	constructor(message?: string) {
		super(10, message || 'unsupported server');
	}
}

export class UnexpectedConnectionState extends LivekitError {
	override readonly name = 'UnexpectedConnectionState';

	constructor(message?: string) {
		super(12, message || 'unexpected connection state');
	}
}

export class NegotiationError extends LivekitError {
	override readonly name = 'NegotiationError';

	constructor(message?: string) {
		super(13, message || 'unable to negotiate');
	}
}

export class PublishDataError extends LivekitError {
	override readonly name = 'PublishDataError';

	constructor(message?: string) {
		super(14, message || 'unable to publish data');
	}
}

export class PublishTrackError extends LivekitError {
	override readonly name = 'PublishTrackError';

	status: number;

	constructor(message: string, status: number) {
		super(15, message);
		this.status = status;
	}
}

export type RequestErrorReason = Exclude<RequestResponse_Reason, RequestResponse_Reason.OK> | 'TimeoutError';

export class SignalRequestError extends LivekitReasonedError<RequestErrorReason> {
	override readonly name = 'SignalRequestError';

	reason: RequestErrorReason;

	reasonName: string;

	constructor(message: string, reason: RequestErrorReason) {
		super(15, message);
		this.reason = reason;
		this.reasonName = typeof reason === 'string' ? reason : RequestResponse_Reason[reason];
	}
}

export enum DataStreamErrorReason {
	AlreadyOpened = 0,

	AbnormalEnd = 1,

	DecodeFailed = 2,

	LengthExceeded = 3,

	Incomplete = 4,

	HandlerAlreadyRegistered = 7,

	EncryptionTypeMismatch = 8,

	HeaderTooLarge = 9,

	PayloadTooLarge = 10,
}

export class DataStreamError extends LivekitReasonedError<DataStreamErrorReason> {
	override readonly name = 'DataStreamError';

	reason: DataStreamErrorReason;

	reasonName: string;

	constructor(message: string, reason: DataStreamErrorReason) {
		super(16, message);
		this.reason = reason;
		this.reasonName = DataStreamErrorReason[reason];
	}
}

export class SignalReconnectError extends LivekitError {
	override readonly name = 'SignalReconnectError';

	constructor(message?: string) {
		super(18, message);
	}
}

export enum MediaDeviceFailure {
	PermissionDenied = 'PermissionDenied',
	NotFound = 'NotFound',
	DeviceInUse = 'DeviceInUse',
	Other = 'Other',
}

export namespace MediaDeviceFailure {
	export function getFailure(error: unknown): MediaDeviceFailure | undefined {
		if (error && typeof error === 'object' && 'name' in error && typeof error.name === 'string') {
			if (error.name === 'NotFoundError' || error.name === 'DevicesNotFoundError') {
				return MediaDeviceFailure.NotFound;
			}
			if (error.name === 'NotAllowedError' || error.name === 'PermissionDeniedError') {
				return MediaDeviceFailure.PermissionDenied;
			}
			if (error.name === 'NotReadableError' || error.name === 'TrackStartError') {
				return MediaDeviceFailure.DeviceInUse;
			}
			return MediaDeviceFailure.Other;
		}
		return undefined;
	}
}
