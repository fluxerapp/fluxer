// SPDX-License-Identifier: AGPL-3.0-or-later

import type {ValidationErrorCode} from '@fluxer/constants/src/ValidationErrorCodes';
import {isValidationErrorCode, ValidationErrorCodes} from '@fluxer/constants/src/ValidationErrorCodes';
import {
	InputValidationError,
	type LocalizedValidationError,
} from '@fluxer/errors/src/domains/core/InputValidationError';
import type {ValidationError} from '@fluxer/errors/src/domains/core/ValidationError';
import type {Context, Env, Input, MiddlewareHandler, TypedResponse, ValidationTargets} from 'hono';
import {getCookie} from 'hono/cookie';
import type {core, input, output, ZodSafeParseResult, ZodType} from 'zod';
import {requireRequestJsonBody} from './utils/RequestJsonBody';
import {initializeFluxerErrorMap} from './ZodErrorMap';

initializeFluxerErrorMap();

function isEmptyObject(obj: object): boolean {
	return Object.keys(obj).length === 0;
}

function getValidationErrorCode(message: string): ValidationErrorCode {
	if (isValidationErrorCode(message)) {
		return message;
	}
	return ValidationErrorCodes.INVALID_FORMAT;
}

function extractVariablesFromIssue(issue: core.$ZodIssue): Record<string, unknown> {
	const path = issue.path;
	const fieldName = path.length > 0 ? String(path[path.length - 1]) : 'field';
	if (issue.code === 'too_small') {
		return {name: fieldName, min: issue.minimum, minValue: issue.minimum};
	}
	if (issue.code === 'too_big') {
		return {name: fieldName, max: issue.maximum, maxLength: issue.maximum, maxValue: issue.maximum};
	}
	if (issue.code === 'invalid_type') {
		return {name: fieldName, expected: issue.expected};
	}
	if (issue.code === 'custom' && issue.params) {
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
type Hook<
	T extends ZodType,
	E extends Env,
	P extends string,
	Target extends keyof ValidationTargets = keyof ValidationTargets,
	V extends Input = Input,
	O = Record<string, unknown>,
> = (
	result: ZodSafeParseResult<output<T>> & {
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
	T extends ZodType,
	E extends Env,
	P extends string,
	Target extends keyof ValidationTargets,
	V extends Input,
> = {
	pre?: PreHook<E, P, Target, V>;
	post?: Hook<T, E, P, Target, V>;
};

export function inputValidationErrorFromZodIssues(issues: Array<core.$ZodIssue>): InputValidationError {
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
	T extends ZodType,
	Target extends keyof ValidationTargets,
	E extends Env,
	P extends string,
	In = input<T>,
	Out = output<T>,
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
				value = await requireRequestJsonBody(c.req);
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
