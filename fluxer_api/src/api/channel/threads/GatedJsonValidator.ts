// SPDX-License-Identifier: AGPL-3.0-or-later

import type {HonoEnv} from '@app/api/types/HonoEnv';
import {readRequestJsonBody} from '@app/api/utils/RequestJsonBody';
import {Validator} from '@app/api/Validator';
import type {Context, MiddlewareHandler} from 'hono';
import type {output, ZodType} from 'zod';

interface GatedJsonValidatorOptions {
	touchesGate: (body: unknown, ctx: Context<HonoEnv>) => boolean;
	active: (ctx: Context<HonoEnv>, body: unknown) => boolean | Promise<boolean>;
	pre?: (raw: unknown, ctx: Context<HonoEnv>) => unknown | Promise<unknown>;
}

export function GatedJsonValidator<Control extends ZodType, Gated extends ZodType>(
	control: Control,
	gated: Gated,
	options: GatedJsonValidatorOptions,
): MiddlewareHandler<HonoEnv, string, {out: {json: output<Control> | output<Gated>}}> {
	const validatorOptions = options.pre ? {pre: options.pre} : undefined;
	const controlValidator = Validator('json', control, validatorOptions) as unknown as MiddlewareHandler<HonoEnv>;
	const gatedValidator = Validator('json', gated, validatorOptions) as unknown as MiddlewareHandler<HonoEnv>;
	return async (ctx, next) => {
		const body = await readRequestJsonBody(ctx.req);
		const useGated = body.parsed && options.touchesGate(body.value, ctx) && (await options.active(ctx, body.value));
		return (useGated ? gatedValidator : controlValidator)(ctx, next);
	};
}
