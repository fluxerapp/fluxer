// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValidationErrorCode} from '@fluxer/constants/src/ValidationErrorCodes';
import {ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	InputValidationError,
	type LocalizedValidationError,
} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ValidationError} from '@fluxer/errors/src/domains/core/ValidationError';
import type {Context, Env, Input, MiddlewareHandler, TypedResponse, ValidationTargets} from 'hono';
import {getCookie} from 'hono/cookie';
import type {ZodError, ZodTypeAny} from 'zod';
import {parseJsonPreservingLargeIntegers} from './utils/LosslessJsonParser';
import {initializeFluxerErrorMap} from './ZodErrorMap';

initializeFluxerErrorMap();

function isEmptyObject(obj: object): boolean {
	return Object.keys(obj).length === 0;
}

const validationErrorCodeSet = new Set<string>(Object.values(ValidationErrorCodes));

function isValidationErrorCode(value: string): value is ValidationErrorCode {
	return validationErrorCodeSet.has(value);
}

function getValidationErrorCode(message: string): ValidationErrorCode {
	if (isValidationErrorCode(message)) {
		return message;
	}
	return ValidationErrorCodes.INVALID_FORMAT;
}

interface ZodTooSmallIssue {
	code: 'too_small';
	minimum: number | bigint;
	type: string;
}

interface ZodTooBigIssue {
	code: 'too_big';
	maximum: number | bigint;
	type: string;
}

function isTooSmallIssue(issue: ZodError['issues'][number]): issue is ZodError['issues'][number] & ZodTooSmallIssue {
	return issue.code === 'too_small' && 'minimum' in issue && 'type' in issue;
}

function isTooBigIssue(issue: ZodError['issues'][number]): issue is ZodError['issues'][number] & ZodTooBigIssue {
	return issue.code === 'too_big' && 'maximum' in issue && 'type' in issue;
}

interface ZodInvalidTypeIssue {
	code: 'invalid_type';
	expected: string;
	received: string;
}

interface ZodCustomIssue {
	code: 'custom';
	params?: Record<string, unknown>;
}

function isInvalidTypeIssue(
	issue: ZodError['issues'][number],
): issue is ZodError['issues'][number] & ZodInvalidTypeIssue {
	return issue.code === 'invalid_type' && 'expected' in issue && 'received' in issue;
}

function isCustomIssue(issue: ZodError['issues'][number]): issue is ZodError['issues'][number] & ZodCustomIssue {
	return issue.code === 'custom';
}

function extractVariablesFromIssue(issue: ZodError['issues'][number]): Record<string, unknown> | undefined {
	const path = issue.path;
	const fieldName = path.length > 0 ? String(path[path.length - 1]) : 'field';
	if (isTooSmallIssue(issue)) {
		return {name: fieldName, min: issue.minimum, minValue: issue.minimum};
	}
	if (isTooBigIssue(issue)) {
		return {name: fieldName, max: issue.maximum, maxLength: issue.maximum, maxValue: issue.maximum};
	}
	if (isInvalidTypeIssue(issue)) {
		return {name: fieldName, expected: issue.expected, received: issue.received};
	}
	if (isCustomIssue(issue) && issue.params) {
		return {name: fieldName, ...issue.params};
	}
	return {name: fieldName};
}

function convertEmptyValuesToNull(obj: unknown, isRoot = true): unknown {
	if (typeof obj === 'string' && obj === '') return null;
	if (Array.isArray(obj)) return obj.map((item) => convertEmptyValuesToNull(item, false));
	if (obj !== null && typeof obj === 'object') {
		if (isEmptyObject(obj) && !isRoot) return null;
		const processed = Object.fromEntries(
			Object.entries(obj).map(([key, value]) => [key, convertEmptyValuesToNull(value, false)]),
		);
		if (!isRoot && Object.values(processed).every((value) => value === null)) return null;
		return processed;
	}
	return obj;
}

type HasUndefined<T> = undefined extends T ? true : false;
type SafeParseResult<T extends ZodTypeAny> =
	| {
			success: true;
			data: T['_output'];
	  }
	| {
			success: false;
			error: ZodError<T['_input']>;
	  };
type Hook<
	T extends ZodTypeAny,
	E extends Env,
	P extends string,
	Target extends keyof ValidationTargets = keyof ValidationTargets,
	V extends Input = Input,
	O = Record<string, unknown>,
> = (
	result: SafeParseResult<T> & {
		target: Target;
	},
	c: Context<E, P, V>,
) => Response | undefined | TypedResponse<O> | Promise<Response | undefined | TypedResponse<O>>;
type PreHook<E extends Env, P extends string, Target extends keyof ValidationTargets, V extends Input> = (
	value: unknown,
	c: Context<E, P, V>,
	target: Target,
) => unknown | Promise<unknown>;
type ValidatorOptions<
	T extends ZodTypeAny,
	E extends Env,
	P extends string,
	Target extends keyof ValidationTargets,
	V extends Input,
> = {
	pre?: PreHook<E, P, Target, V>;
	post?: Hook<T, E, P, Target, V>;
};

export function inputValidationErrorFromZodIssues(issues: ZodError['issues']): InputValidationError {
	const errors: Array<ValidationError> = [];
	const localizedErrors: Array<LocalizedValidationError> = [];
	const seen = new Set<string>();
	for (const issue of issues) {
		const path = issue.path.length > 0 ? issue.path.map(String).join('.') : 'root';
		const code = getValidationErrorCode(issue.message);
		const key = `${path}|${code}`;
		if (seen.has(key)) continue;
		seen.add(key);
		const variables = extractVariablesFromIssue(issue);
		errors.push({path, message: code, code});
		localizedErrors.push({path, code, variables});
	}
	return new InputValidationError(errors, localizedErrors);
}

export const Validator = <
	T extends ZodTypeAny,
	Target extends keyof ValidationTargets,
	E extends Env,
	P extends string,
	In = T['_input'],
	Out = T['_output'],
	I extends Input = {
		in: HasUndefined<In> extends true
			? {
					[K in Target]?: In extends ValidationTargets[K]
						? In
						: {
								[K2 in keyof In]?: ValidationTargets[K][K2];
							};
				}
			: {
					[K in Target]: In extends ValidationTargets[K]
						? In
						: {
								[K2 in keyof In]: ValidationTargets[K][K2];
							};
				};
		out: {
			[K in Target]: Out;
		};
	},
	V extends I = I,
>(
	target: Target,
	schema: T,
	hookOrOptions?: Hook<T, E, P, Target, V> | ValidatorOptions<T, E, P, Target, V>,
): MiddlewareHandler<E, P, V> => {
	const options: ValidatorOptions<T, E, P, Target, V> =
		typeof hookOrOptions === 'function' ? {post: hookOrOptions} : (hookOrOptions ?? {});
	return async (c, next): Promise<Response | undefined> => {
		let value: unknown;
		switch (target) {
			case 'json':
				try {
					const raw = await c.req.text();
					value = raw.trim().length === 0 ? {} : parseJsonPreservingLargeIntegers(raw);
				} catch {
					value = {};
				}
				break;
			case 'form': {
				const formData = await c.req.formData();
				type FormDataEntry = File | string;
				type FormValue = FormDataEntry | Array<FormDataEntry>;
				const form: Record<string, FormValue> = {};
				formData.forEach((formValue, key) => {
					const existingValue = form[key];
					if (key.endsWith('[]')) {
						const list = Array.isArray(existingValue)
							? existingValue
							: existingValue !== undefined
								? [existingValue]
								: [];
						list.push(formValue);
						form[key] = list;
					} else if (Array.isArray(existingValue)) {
						existingValue.push(formValue);
					} else if (existingValue !== undefined) {
						form[key] = [existingValue, formValue];
					} else {
						form[key] = formValue;
					}
				});
				value = form;
				break;
			}
			case 'query':
				value = Object.fromEntries(
					Object.entries(c.req.queries()).map(([k, v]) => (v.length === 1 ? [k, v[0]] : [k, v])),
				);
				break;
			case 'param':
				value = c.req.param();
				break;
			case 'header':
				value = c.req.header();
				break;
			case 'cookie':
				value = getCookie(c);
				break;
			default:
				value = {};
		}
		if (options.pre) {
			value = await options.pre(value, c, target);
		}
		const transformedValue = convertEmptyValuesToNull(value);
		const result = await schema.safeParseAsync(transformedValue);
		if (options.post) {
			const hookResult = await options.post({...result, target}, c);
			if (hookResult) {
				if (hookResult instanceof Response) return hookResult;
				if ('response' in hookResult && hookResult.response instanceof Response) return hookResult.response;
			}
		}
		if (!result.success) {
			throw inputValidationErrorFromZodIssues(result.error.issues);
		}
		c.req.addValidatedData(target, result.data as ValidationTargets[Target]);
		await next();
		return;
	};
};
