// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {HttpStatus} from '@fluxer/constants/src/HttpConstants';
import {FluxerError, type FluxerErrorData} from '@fluxer/errors/src/FluxerError';

interface HttpErrorOptions {
	code?: string;
	message?: string;
	data?: FluxerErrorData;
	headers?: Record<string, string>;
	cause?: Error;
}

export class BadRequestError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.BAD_REQUEST,
			message: options.message ?? 'Bad Request',
			status: HttpStatus.BAD_REQUEST,
		});
		this.name = 'BadRequestError';
	}
}

export class UnauthorizedError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.UNAUTHORIZED,
			message: options.message ?? 'Unauthorized',
			status: HttpStatus.UNAUTHORIZED,
		});
		this.name = 'UnauthorizedError';
	}
}

export class ForbiddenError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.FORBIDDEN,
			message: options.message ?? 'Forbidden',
			status: HttpStatus.FORBIDDEN,
		});
		this.name = 'ForbiddenError';
	}
}

export class NotFoundError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.NOT_FOUND,
			message: options.message ?? 'Not Found',
			status: HttpStatus.NOT_FOUND,
		});
		this.name = 'NotFoundError';
	}
}

export class MethodNotAllowedError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.METHOD_NOT_ALLOWED,
			message: options.message ?? 'Method Not Allowed',
			status: HttpStatus.METHOD_NOT_ALLOWED,
		});
		this.name = 'MethodNotAllowedError';
	}
}

export class ConflictError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.CONFLICT,
			message: options.message ?? 'Conflict',
			status: HttpStatus.CONFLICT,
		});
		this.name = 'ConflictError';
	}
}

export class GoneError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.GONE,
			message: options.message ?? 'Gone',
			status: HttpStatus.GONE,
		});
		this.name = 'GoneError';
	}
}

export class InternalServerError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.INTERNAL_SERVER_ERROR,
			message: options.message ?? 'Internal Server Error',
			status: HttpStatus.INTERNAL_SERVER_ERROR,
		});
		this.name = 'InternalServerError';
	}
}

export class NotImplementedError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.NOT_IMPLEMENTED,
			message: options.message ?? 'Not Implemented',
			status: HttpStatus.NOT_IMPLEMENTED,
		});
		this.name = 'NotImplementedError';
	}
}

export class ServiceUnavailableError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.SERVICE_UNAVAILABLE,
			message: options.message ?? 'Service Unavailable',
			status: HttpStatus.SERVICE_UNAVAILABLE,
		});
		this.name = 'ServiceUnavailableError';
	}
}

export class BadGatewayError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.BAD_GATEWAY,
			message: options.message ?? 'Bad Gateway',
			status: HttpStatus.BAD_GATEWAY,
		});
		this.name = 'BadGatewayError';
	}
}

export class GatewayTimeoutError extends FluxerError {
	constructor(options: HttpErrorOptions = {}) {
		super({
			...options,
			code: options.code ?? APIErrorCodes.GATEWAY_TIMEOUT,
			message: options.message ?? 'Gateway Timeout',
			status: HttpStatus.GATEWAY_TIMEOUT,
		});
		this.name = 'GatewayTimeoutError';
	}
}
