/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {Context, Env, Input, MiddlewareHandler, TypedResponse, ValidationTargets} from 'hono';
import {getCookie} from 'hono/cookie';
import type {ZodError, ZodType} from 'zod';
import {InputValidationError, type ValidationError} from '~/Errors';

const isEmptyObject = (obj: object): boolean => Object.keys(obj).length === 0;

const convertEmptyValuesToNull = (obj: unknown, isRoot = true): unknown => {
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
};

type HasUndefined<T> = undefined extends T ? true : false;
type SafeParseResult<T extends ZodType> =
	| {success: true; data: T['_output']}
	| {success: false; error: ZodError<T['_input']>};

type Hook<
	T extends ZodType,
	E extends Env,
	P extends string,
	Target extends keyof ValidationTargets = keyof ValidationTargets,
	V extends Input = Input,
	O = Record<string, unknown>,
> = (
	result: SafeParseResult<T> & {target: Target},
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

export const Validator = <
	T extends ZodType,
	Target extends keyof ValidationTargets,
	E extends Env,
	P extends string,
	In = T['_input'],
	Out = T['_output'],
	I extends Input = {
		in: HasUndefined<In> extends true
			? {[K in Target]?: In extends ValidationTargets[K] ? In : {[K2 in keyof In]?: ValidationTargets[K][K2]}}
			: {[K in Target]: In extends ValidationTargets[K] ? In : {[K2 in keyof In]: ValidationTargets[K][K2]}};
		out: {[K in Target]: Out};
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
					value = await c.req.json<unknown>();
				} catch {
					value = {};
				}
				break;
			case 'form': {
				const formData = await c.req.formData();
				type FormValue = FormDataEntryValue | Array<FormDataEntryValue>;
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
			const errors: Array<ValidationError> = [];
			for (const issue of result.error.issues) {
				const path = issue.path.length > 0 ? issue.path.join('.') : 'root';
				errors.push({path, message: issue.message});
			}
			throw new InputValidationError(errors);
		}

		c.req.addValidatedData(target, result.data as ValidationTargets[Target]);
		await next();
		return;
	};
};
