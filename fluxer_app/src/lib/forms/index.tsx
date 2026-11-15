// SPDX-License-Identifier: AGPL-3.0-or-later

import {GenericErrorModal} from '@app/features/app/components/alerts/GenericErrorModal';
import {SOMETHING_WENT_WRONG_DESCRIPTOR} from '@app/features/i18n/utils/CommonMessageDescriptors';
import type {HttpError} from '@app/features/platform/types/EndpointError';
import type {RestResponse} from '@app/features/platform/types/TransportTypes';
import {isAbortError} from '@app/features/platform/utils/PlatformHelpers';
import * as ModalCommands from '@app/features/ui/commands/ModalCommands';
import {modal} from '@app/features/ui/commands/ModalCommands';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';
import type {I18n} from '@lingui/core';
import {msg} from '@lingui/core/macro';
import type {FieldValues, Path, UseFormReturn} from 'react-hook-form';

const AN_UNEXPECTED_ERROR_OCCURRED_PLEASE_TRY_AGAIN_DESCRIPTOR = msg({
	message: 'An unexpected error occurred. Try again.',
	comment: 'Generic error toast body asking the user to retry.',
});

interface ValidationError {
	path: string;
	message: string;
}

interface APIErrorResponse {
	code: string;
	message: string;
	errors?: Array<ValidationError>;
}

interface HandleErrorOptions<T extends FieldValues> {
	pathMap?: Partial<Record<string, Path<T>>>;
}

function collectFormPaths(value: unknown, prefix: string, paths: Set<string>): void {
	if (Array.isArray(value)) {
		paths.add(prefix);
		for (let i = 0; i < value.length; i++) {
			collectFormPaths(value[i], `${prefix}.${i}`, paths);
		}
		return;
	}
	if (value && typeof value === 'object') {
		for (const [key, child] of Object.entries(value)) {
			const next = prefix ? `${prefix}.${key}` : key;
			paths.add(next);
			collectFormPaths(child, next, paths);
		}
		return;
	}
	if (prefix) {
		paths.add(prefix);
	}
}

function toCamelCaseSegment(value: string): string {
	return value.replace(/_([a-z])/g, (_match, char: string) => char.toUpperCase());
}

function toCamelCasePath(path: string): string {
	if (!path.includes('_') && !path.includes('.')) {
		return path;
	}
	return path
		.split('.')
		.map((segment) => (/^\d+$/.test(segment) ? segment : toCamelCaseSegment(segment)))
		.join('.');
}

function resolveMappedPath<T extends FieldValues>(
	pathMap: HandleErrorOptions<T>['pathMap'],
	rawPath: string,
): string | null {
	if (!pathMap) return null;
	const direct = pathMap[rawPath];
	if (direct) return String(direct);
	for (const [sourcePath, targetPath] of Object.entries(pathMap)) {
		if (rawPath.startsWith(`${sourcePath}.`)) {
			return `${String(targetPath)}${rawPath.slice(sourcePath.length)}`;
		}
	}
	return null;
}

export function handleError<T extends FieldValues>(
	i18n: I18n,
	form: UseFormReturn<T>,
	error: RestResponse<unknown> | HttpError,
	defaultPath: Path<T>,
	options?: HandleErrorOptions<T>,
) {
	const errorBody = 'body' in error ? error.body : undefined;
	if (errorBody) {
		const errorData = errorBody as APIErrorResponse;
		if (errorData.code === APIErrorCodes.INVALID_FORM_BODY && errorData.errors?.length) {
			const formPaths = new Set<string>();
			collectFormPaths(form.getValues(), '', formPaths);
			const resolvedMessages = new Map<string, string>();
			const unknownMessages: Array<string> = [];
			for (const validationError of errorData.errors) {
				const rawPath = validationError.path;
				const message = validationError.message;
				const mappedPath = resolveMappedPath(options?.pathMap, rawPath);
				const candidates = [mappedPath ? String(mappedPath) : null, rawPath, toCamelCasePath(rawPath)].filter(
					Boolean,
				) as Array<string>;
				const resolvedPath = candidates.find((candidate) => formPaths.has(candidate)) ?? null;
				if (resolvedPath) {
					const existing = resolvedMessages.get(resolvedPath);
					resolvedMessages.set(resolvedPath, existing ? `${existing} ${message}` : message);
				} else {
					unknownMessages.push(message);
				}
			}
			if (unknownMessages.length > 0) {
				const uniqueUnknown = Array.from(new Set(unknownMessages));
				const unknownCombined = uniqueUnknown.join(' ');
				const defaultKey = String(defaultPath);
				const existing = resolvedMessages.get(defaultKey);
				resolvedMessages.set(defaultKey, existing ? `${existing} ${unknownCombined}` : unknownCombined);
			}
			for (const [path, message] of resolvedMessages) {
				form.setError(path as Path<T>, {type: 'server', message});
			}
		} else if (errorData.message) {
			form.setError(defaultPath, {type: 'server', message: errorData.message});
		}
		return;
	}
	form.setError(defaultPath, {
		type: 'server',
		message: i18n._(AN_UNEXPECTED_ERROR_OCCURRED_PLEASE_TRY_AGAIN_DESCRIPTOR),
	});
}

export function extractErrorMessage(i18n: I18n, error: unknown): string {
	let errorBody: unknown;
	if (error && typeof error === 'object' && 'body' in error) {
		errorBody = (error as HttpError | RestResponse<unknown>).body;
	}
	if (errorBody) {
		const errorData = errorBody as APIErrorResponse;
		if (errorData.code === APIErrorCodes.INVALID_FORM_BODY && errorData.errors?.length) {
			const messages = Array.from(new Set(errorData.errors.map((e) => e.message))).join(' ');
			if (messages) return messages;
		}
		if (errorData.message) return errorData.message;
	}
	return i18n._(AN_UNEXPECTED_ERROR_OCCURRED_PLEASE_TRY_AGAIN_DESCRIPTOR);
}

export function pushApiErrorModal(i18n: I18n, error: unknown, title?: string): void {
	if (isAbortError(error)) return;
	const message = extractErrorMessage(i18n, error);
	const modalTitle = title ?? i18n._(SOMETHING_WENT_WRONG_DESCRIPTOR);
	ModalCommands.push(
		modal(() => (
			<GenericErrorModal
				title={modalTitle}
				message={message}
				data-flx="lib.forms.push-api-error-modal.generic-error-modal"
			/>
		)),
	);
}
