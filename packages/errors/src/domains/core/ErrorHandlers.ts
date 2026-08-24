// SPDX-License-Identifier: AGPL-3.0-or-later

import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import {Locales} from '@fluxer/constants/src/Locales';
import {OAuth2Error} from '@fluxer/errors/src/domains/auth/OAuth2Error';
import {
	getErrorRecord,
	hasApiErrorCode,
	resolveApiErrorCode,
	resolveErrorData,
	resolveErrorHeaders,
	resolveErrorMessage,
	resolveErrorStatus,
	resolveMessageVariables,
} from '@fluxer/errors/src/error_handling/ErrorIntrospection';
import {createJsonErrorResponse} from '@fluxer/errors/src/error_handling/ErrorResponse';
import {FluxerError} from '@fluxer/errors/src/FluxerError';
import {ErrorCodeToI18nKey} from '@fluxer/errors/src/i18n/ErrorCodeMappings';
import {getErrorMessageUnsafe} from '@fluxer/errors/src/i18n/ErrorI18n';
import type {ErrorI18nKey} from '@fluxer/errors/src/i18n/ErrorI18nMessages';
import type {BaseHonoEnv, ErrorI18nService} from '@fluxer/hono_types/src/HonoTypes';
import {createLogger} from '@fluxer/logger/src/Logger';
import type {Context} from 'hono';
import {HTTPException} from 'hono/http-exception';

const logger = createLogger('errors');
const LOCALE_LOOKUP = new Map<string, string>(Object.values(Locales).map((locale) => [locale.toLowerCase(), locale]));

interface LocalizedValidationErrorEntry {
	path: string;
	code: string;
	variables?: Record<string, unknown>;
}

function isRecord(value: unknown): value is Record<string, unknown> {
	return value !== null && typeof value === 'object';
}

function isLocalizedValidationErrorEntry(value: unknown): value is LocalizedValidationErrorEntry {
	if (!isRecord(value)) {
		return false;
	}
	const variables = value.variables;
	return (
		typeof value.path === 'string' && typeof value.code === 'string' && (variables === undefined || isRecord(variables))
	);
}

function getLocalizedValidationErrors(err: unknown): Array<LocalizedValidationErrorEntry> | null {
	if (!isRecord(err)) {
		return null;
	}
	const errors = err.localizedErrors;
	if (errors === null || errors === undefined) {
		return null;
	}
	if (!Array.isArray(errors) || !errors.every(isLocalizedValidationErrorEntry)) {
		return null;
	}
	return errors;
}

function isExpectedError(err: Error): boolean {
	return 'isExpected' in err && Boolean(err.isExpected);
}

function resolveLocaleFromAcceptLanguage(acceptLanguageHeader: string | undefined): string | undefined {
	if (!acceptLanguageHeader) {
		return undefined;
	}
	for (const entry of acceptLanguageHeader.split(',')) {
		const localeToken = entry.split(';')[0]?.trim().toLowerCase();
		if (!localeToken) {
			continue;
		}
		const exactMatch = LOCALE_LOOKUP.get(localeToken);
		if (exactMatch) {
			return exactMatch;
		}
		const baseToken = localeToken.split('-')[0];
		if (!baseToken) {
			continue;
		}
		if (baseToken === 'en') {
			return Locales.EN_US;
		}
		const baseMatch = LOCALE_LOOKUP.get(baseToken);
		if (baseMatch) {
			return baseMatch;
		}
	}
	return undefined;
}

function getI18nContext<E extends BaseHonoEnv>(
	ctx: Context<E>,
): {
	errorI18nService: ErrorI18nService | undefined;
	locale: string;
} {
	const requestLocale = ctx.get('requestLocale');
	const locale = requestLocale ?? resolveLocaleFromAcceptLanguage(ctx.req.header('accept-language')) ?? Locales.EN_US;
	return {
		errorI18nService: ctx.get('errorI18nService'),
		locale,
	};
}

function resolveLocalizedMessageWithoutService(
	code: string,
	locale: string,
	variables: Record<string, unknown> | undefined,
	fallbackMessage: string | undefined,
): string {
	const i18nKey = ErrorCodeToI18nKey[code as keyof typeof ErrorCodeToI18nKey] ?? (code as ErrorI18nKey);
	return getErrorMessageUnsafe(i18nKey, locale, variables, fallbackMessage);
}

function resolveLocalizedMessage(
	errorI18nService: ErrorI18nService | undefined,
	code: string,
	locale: string,
	variables: Record<string, unknown> | undefined,
	fallbackMessage: string | undefined,
): string {
	if (errorI18nService) {
		return errorI18nService.getMessage(code, locale, variables, fallbackMessage);
	}
	return resolveLocalizedMessageWithoutService(code, locale, variables, fallbackMessage);
}

function handleLocalizedValidationErrors<E extends BaseHonoEnv>(err: unknown, ctx: Context<E>): Response | null {
	const localizedErrors = getLocalizedValidationErrors(err);
	if (!localizedErrors) {
		return null;
	}
	try {
		const {errorI18nService, locale} = getI18nContext(ctx);
		const resolvedErrors = localizedErrors.map((e) => ({
			path: e.path,
			message: resolveLocalizedMessage(errorI18nService, e.code, locale, e.variables, e.code),
			code: e.code,
		}));
		logger.debug({locale, errors: resolvedErrors}, 'Resolved localized validation errors');
		const localizedMessage = resolveLocalizedMessage(
			errorI18nService,
			APIErrorCodes.INVALID_FORM_BODY,
			locale,
			undefined,
			'Input Validation Error',
		);
		return createJsonErrorResponse({
			status: 400,
			code: APIErrorCodes.INVALID_FORM_BODY,
			message: localizedMessage,
			data: {errors: resolvedErrors},
		});
	} catch {
		logger.warn({err}, 'Failed to resolve localized validation errors, falling back to default error response');
		return null;
	}
}

function handleFluxerError<E extends BaseHonoEnv>(err: FluxerError, ctx: Context<E>): Response {
	const localizedResponse = handleLocalizedValidationErrors(err, ctx);
	if (localizedResponse) {
		return localizedResponse;
	}
	const {errorI18nService, locale} = getI18nContext(ctx);
	const resolvedMessage = resolveLocalizedMessage(
		errorI18nService,
		err.code,
		locale,
		err.messageVariables,
		err.message,
	);
	return createJsonErrorResponse({
		status: err.status,
		code: err.code,
		message: resolvedMessage,
		data: err.data,
		headers: err.headers,
	});
}

function handleKnownErrorCode<E extends BaseHonoEnv>(err: unknown, errorCode: string, ctx: Context<E>): Response {
	const {errorI18nService, locale} = getI18nContext(ctx);
	const resolvedMessage = resolveLocalizedMessage(
		errorI18nService,
		errorCode,
		locale,
		resolveMessageVariables(err),
		resolveErrorMessage(err),
	);
	const status = resolveErrorStatus(err) ?? (errorCode === APIErrorCodes.GENERAL_ERROR ? 500 : 400);
	return createJsonErrorResponse({
		status,
		code: errorCode,
		message: resolvedMessage,
		data: resolveErrorData(err),
		headers: resolveErrorHeaders(err),
	});
}

const HTTP_STATUS_TO_ERROR_CODE: Partial<Record<number, string>> = {
	400: APIErrorCodes.BAD_REQUEST,
	403: APIErrorCodes.FORBIDDEN,
	404: APIErrorCodes.NOT_FOUND,
	405: APIErrorCodes.METHOD_NOT_ALLOWED,
	409: APIErrorCodes.CONFLICT,
	410: APIErrorCodes.GONE,
	500: APIErrorCodes.INTERNAL_SERVER_ERROR,
	501: APIErrorCodes.NOT_IMPLEMENTED,
	502: APIErrorCodes.BAD_GATEWAY,
	503: APIErrorCodes.SERVICE_UNAVAILABLE,
	504: APIErrorCodes.GATEWAY_TIMEOUT,
};

function handleHTTPException<E extends BaseHonoEnv>(err: HTTPException, ctx: Context<E>): Response {
	const errorRecord = getErrorRecord(err);
	if (errorRecord && typeof errorRecord.code === 'string' && hasApiErrorCode(errorRecord.code)) {
		const {errorI18nService, locale} = getI18nContext(ctx);
		const resolvedMessage = resolveLocalizedMessage(
			errorI18nService,
			errorRecord.code,
			locale,
			resolveMessageVariables(err),
			err.message,
		);
		return createJsonErrorResponse({
			status: err.status,
			code: errorRecord.code,
			message: resolvedMessage,
			data: resolveErrorData(err),
			headers: resolveErrorHeaders(err),
		});
	}
	const code = HTTP_STATUS_TO_ERROR_CODE[err.status] ?? APIErrorCodes.GENERAL_ERROR;
	const {errorI18nService, locale} = getI18nContext(ctx);
	const resolvedMessage = resolveLocalizedMessage(errorI18nService, code, locale, undefined, err.message);
	return createJsonErrorResponse({
		status: err.status,
		code,
		message: resolvedMessage,
	});
}

function handleUnexpectedError<E extends BaseHonoEnv>(ctx: Context<E>): Response {
	const code = APIErrorCodes.INTERNAL_SERVER_ERROR;
	const {errorI18nService, locale} = getI18nContext(ctx);
	const resolvedMessage = resolveLocalizedMessage(errorI18nService, code, locale, undefined, undefined);
	return createJsonErrorResponse({
		status: 500,
		code,
		message: resolvedMessage,
	});
}

interface ResolvedErrorResponse {
	response: Response;
	unexpected: boolean;
}

function resolveErrorResponse<E extends BaseHonoEnv>(err: Error, ctx: Context<E>): ResolvedErrorResponse {
	if (err instanceof OAuth2Error) {
		return {response: err.getResponse(), unexpected: false};
	}
	if (err instanceof FluxerError) {
		return {response: handleFluxerError(err, ctx), unexpected: false};
	}
	const errorCode = resolveApiErrorCode(err);
	if (errorCode) {
		return {response: handleKnownErrorCode(err, errorCode, ctx), unexpected: false};
	}
	if (err instanceof HTTPException) {
		return {response: handleHTTPException(err, ctx), unexpected: false};
	}
	if (isExpectedError(err)) {
		return {
			response: createJsonErrorResponse({
				status: 400,
				code: APIErrorCodes.GENERAL_ERROR,
				message: err.message,
			}),
			unexpected: false,
		};
	}
	return {response: handleUnexpectedError(ctx), unexpected: true};
}

function logErrorResponse<E extends BaseHonoEnv>(err: Error, resolved: ResolvedErrorResponse, ctx: Context<E>): void {
	const status = resolved.response.status;
	const details = {
		err,
		status,
		method: ctx.req.method,
		path: ctx.req.path,
		requestId: ctx.get('requestId'),
	};
	if (resolved.unexpected) {
		logger.error(details, 'Unhandled error occurred');
		return;
	}
	if (status >= 500) {
		logger.error(details, 'Request failed');
		return;
	}
	logger.debug(details, 'Request rejected');
}

export function AppErrorHandler<E extends BaseHonoEnv = BaseHonoEnv>(
	err: Error,
	ctx: Context<E>,
): Response | Promise<Response> {
	const resolved = resolveErrorResponse(err, ctx);
	logErrorResponse(err, resolved, ctx);
	return resolved.response;
}

export function AppNotFoundHandler<E extends BaseHonoEnv = BaseHonoEnv>(ctx: Context<E>): Response | Promise<Response> {
	const code = APIErrorCodes.NOT_FOUND;
	const {errorI18nService, locale} = getI18nContext(ctx);
	const resolvedMessage = resolveLocalizedMessage(errorI18nService, code, locale, undefined, undefined);
	return createJsonErrorResponse({
		status: 404,
		code,
		message: resolvedMessage,
	});
}
