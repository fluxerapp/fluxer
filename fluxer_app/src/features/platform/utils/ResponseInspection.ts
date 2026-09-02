// SPDX-License-Identifier: AGPL-3.0-or-later

import {HttpError} from '@app/features/platform/types/EndpointError';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';

const DEFAULT_IP_AUTHORIZATION_RESEND_SECONDS = 30;

const NON_CONTEXT_KEYS = new Set(['code', 'details', 'errors', 'message', 'request_id', 'retry_after', 'status']);

interface ValidationFault {
	path: string;
	code?: string;
	message: string;
}

export interface ApiErrorResponse {
	status: number | undefined;
	code: string | undefined;
	message: string | undefined;
	requestId: string | undefined;
	fields: ReadonlyArray<ValidationFault> | undefined;
	retryAfterSeconds: number | undefined;
	context: Record<string, unknown> | undefined;
}

export interface IpAuthorizationRequiredResponse {
	ip_authorization_required: true;
	ticket: string;
	email: string;
	resend_available_in: number;
}

export function parseAPIErrorResponse(body: unknown, httpStatus?: number): ApiErrorResponse | null {
	if (!isRecord(body)) return null;
	const details = isRecord(body.details) ? body.details : undefined;
	const retry = details != null && isRecord(details.retry) ? details.retry : undefined;
	return {
		status: httpStatus ?? readNumber(body, 'status'),
		code: readString(body, 'code'),
		message: readString(body, 'message'),
		requestId: readString(body, 'request_id'),
		fields: readValidationFaults(details?.fields) ?? readValidationFaults(body.errors),
		retryAfterSeconds: readNumber(retry, 'after_seconds') ?? readNumber(body, 'retry_after'),
		context: readErrorContext(body, details),
	};
}

export function replyCode(body: unknown): string | undefined {
	return parseAPIErrorResponse(body)?.code;
}

export function replyMessage(body: unknown): string | undefined {
	return parseAPIErrorResponse(body)?.message;
}

export function replyRetryAfter(body: unknown): number | undefined {
	return parseAPIErrorResponse(body)?.retryAfterSeconds;
}

export function failureCode(error: unknown): string | undefined {
	return failureResponse(error)?.code;
}

export function failureMessage(error: unknown): string | undefined {
	return failureResponse(error)?.message;
}

export function failureRetryAfter(error: unknown): number | undefined {
	return failureResponse(error)?.retryAfterSeconds;
}

export function failureValidationErrors(error: unknown): ReadonlyArray<ValidationFault> | undefined {
	return failureResponse(error)?.fields;
}

export function ipAuthorizationRequiredResponseFromError(error: unknown): IpAuthorizationRequiredResponse | null {
	const response = failureResponse(error);
	if (response == null || response.status !== HttpStatus.FORBIDDEN) return null;
	if (response.code !== APIErrorCodes.IP_AUTHORIZATION_REQUIRED) return null;
	const context = response.context;
	if (context == null) return null;
	const ticket = readString(context, 'ticket');
	const email = readString(context, 'email');
	if (ticket == null || ticket.length === 0 || email == null || email.length === 0) return null;
	const resendAvailableIn = readNumber(context, 'resend_available_in');
	if (resendAvailableIn != null && (!Number.isSafeInteger(resendAvailableIn) || resendAvailableIn < 0)) return null;
	return {
		ip_authorization_required: true,
		ticket,
		email,
		resend_available_in: resendAvailableIn ?? DEFAULT_IP_AUTHORIZATION_RESEND_SECONDS,
	};
}

function failureResponse(error: unknown): ApiErrorResponse | null {
	return error instanceof HttpError ? parseAPIErrorResponse(error.body, error.status) : null;
}

function readErrorContext(
	body: Record<string, unknown>,
	details: Record<string, unknown> | undefined,
): Record<string, unknown> | undefined {
	if (details != null && isRecord(details.context)) return details.context;
	const context: Record<string, unknown> = {};
	for (const [key, value] of Object.entries(body)) {
		if (NON_CONTEXT_KEYS.has(key)) continue;
		context[key] = value;
	}
	return Object.keys(context).length > 0 ? context : undefined;
}

function readValidationFaults(value: unknown): ReadonlyArray<ValidationFault> | undefined {
	if (!Array.isArray(value)) return undefined;
	const faults: Array<ValidationFault> = [];
	for (const entry of value) {
		const path = readString(entry, 'path');
		const message = readString(entry, 'message');
		if (path == null || message == null) continue;
		const code = readString(entry, 'code');
		faults.push(code == null ? {path, message} : {path, code, message});
	}
	return faults.length > 0 ? faults : undefined;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return typeof value === 'object' && value !== null;
}

function readString(value: unknown, key: string): string | undefined {
	if (!isRecord(value)) return undefined;
	const found = value[key];
	return typeof found === 'string' ? found : undefined;
}

function readNumber(value: unknown, key: string): number | undefined {
	if (!isRecord(value)) return undefined;
	const found = value[key];
	return typeof found === 'number' ? found : undefined;
}
