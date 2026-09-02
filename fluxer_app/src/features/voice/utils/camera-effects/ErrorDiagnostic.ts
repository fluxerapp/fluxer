// SPDX-License-Identifier: AGPL-3.0-or-later

export const ErrorDiagnosticType = Object.freeze({
	AGGREGATE_ERROR: 'aggregate-error',
	DOM_EXCEPTION: 'dom-exception',
	ERROR: 'error',
	NON_ERROR: 'non-error',
} as const);

export type ErrorDiagnosticType = (typeof ErrorDiagnosticType)[keyof typeof ErrorDiagnosticType];

export interface ErrorDiagnostic {
	readonly errorType: ErrorDiagnosticType;
	readonly message: string;
	readonly stack: string | null;
}

const ERROR_DIAGNOSTIC_ENTRY_MAX = 16;
export const ERROR_DIAGNOSTIC_MESSAGE_MAX_LENGTH = 16_384;
export const ERROR_DIAGNOSTIC_STACK_MAX_LENGTH = 65_536;
const ERROR_DIAGNOSTIC_NAME_MAX_LENGTH = 128;
const ERROR_DIAGNOSTIC_MESSAGE_ENTRY_MAX_LENGTH = 896;
const ERROR_DIAGNOSTIC_STACK_ENTRY_MAX_LENGTH = 4_096;

export function isErrorDiagnosticType(value: unknown): value is ErrorDiagnosticType {
	if (value === ErrorDiagnosticType.AGGREGATE_ERROR) {
		return true;
	}
	if (value === ErrorDiagnosticType.DOM_EXCEPTION) {
		return true;
	}
	if (value === ErrorDiagnosticType.ERROR) {
		return true;
	}
	return value === ErrorDiagnosticType.NON_ERROR;
}

export function isErrorDiagnostic(value: object): value is ErrorDiagnostic {
	const errorType = Reflect.get(value, 'errorType');
	if (!isErrorDiagnosticType(errorType)) {
		return false;
	}
	const message = Reflect.get(value, 'message');
	if (typeof message !== 'string') {
		return false;
	}
	if (message.length > ERROR_DIAGNOSTIC_MESSAGE_MAX_LENGTH) {
		return false;
	}
	const stack = Reflect.get(value, 'stack');
	if (stack === null) {
		return true;
	}
	if (typeof stack !== 'string') {
		return false;
	}
	return stack.length <= ERROR_DIAGNOSTIC_STACK_MAX_LENGTH;
}

export function getErrorDiagnosticType(error: unknown): ErrorDiagnosticType {
	if (error instanceof AggregateError) {
		return ErrorDiagnosticType.AGGREGATE_ERROR;
	}
	if (error instanceof DOMException) {
		return ErrorDiagnosticType.DOM_EXCEPTION;
	}
	if (error instanceof Error) {
		return ErrorDiagnosticType.ERROR;
	}
	return ErrorDiagnosticType.NON_ERROR;
}

function limitDiagnosticText(value: string, maximumLength: number): string {
	if (value.length <= maximumLength) {
		return value;
	}
	return value.slice(0, maximumLength);
}

function describeError(error: unknown): string {
	if (error instanceof Error) {
		const name = limitDiagnosticText(error.name, ERROR_DIAGNOSTIC_NAME_MAX_LENGTH);
		const message = limitDiagnosticText(error.message, ERROR_DIAGNOSTIC_MESSAGE_ENTRY_MAX_LENGTH);
		return `${name}: ${message}`;
	}
	try {
		return limitDiagnosticText(String(error), ERROR_DIAGNOSTIC_MESSAGE_ENTRY_MAX_LENGTH);
	} catch {
		return 'Unprintable non-error failure';
	}
}

function collectDiagnosticErrors(error: unknown): ReadonlyArray<unknown> {
	const collected: Array<unknown> = [error];
	let index = 0;
	while (index < collected.length) {
		if (collected.length >= ERROR_DIAGNOSTIC_ENTRY_MAX) {
			break;
		}
		const current = collected[index];
		index += 1;
		if (current instanceof Error) {
			const cause = current.cause;
			if (cause !== undefined) {
				collected.push(cause);
			}
		}
		if (!(current instanceof AggregateError)) {
			continue;
		}
		if (!Array.isArray(current.errors)) {
			continue;
		}
		for (const nested of current.errors) {
			if (collected.length >= ERROR_DIAGNOSTIC_ENTRY_MAX) {
				break;
			}
			collected.push(nested);
		}
	}
	return collected;
}

function hasErrorStack(error: unknown): error is Error {
	if (!(error instanceof Error)) {
		return false;
	}
	return typeof error.stack === 'string';
}

export function getErrorDiagnostic(error: unknown): ErrorDiagnostic {
	const errors = collectDiagnosticErrors(error);
	const message = errors
		.map((current, index) => (index === 0 ? describeError(current) : `Cause ${index}: ${describeError(current)}`))
		.join('\n');
	const stack = errors
		.filter(hasErrorStack)
		.map((current, index) => {
			const entry = limitDiagnosticText(current.stack ?? '', ERROR_DIAGNOSTIC_STACK_ENTRY_MAX_LENGTH);
			return index === 0 ? entry : `Caused by:\n${entry}`;
		})
		.join('\n');
	return {
		errorType: getErrorDiagnosticType(error),
		message: limitDiagnosticText(message, ERROR_DIAGNOSTIC_MESSAGE_MAX_LENGTH),
		stack: stack.length === 0 ? null : limitDiagnosticText(stack, ERROR_DIAGNOSTIC_STACK_MAX_LENGTH),
	};
}
