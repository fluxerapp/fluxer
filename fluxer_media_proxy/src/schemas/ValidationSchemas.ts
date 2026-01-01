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

import * as v from 'valibot';

export const ImageParamSchema = v.object({
	id: v.string(),
	filename: v.pipe(
		v.string(),
		v.minLength(1),
		v.maxLength(100),
		v.regex(/^[a-zA-Z0-9_]+\.[a-zA-Z0-9]+$/, 'Invalid filename'),
	),
});

export const ImageQuerySchema = v.object({
	size: v.optional(
		v.picklist([
			'16',
			'20',
			'22',
			'24',
			'28',
			'32',
			'40',
			'44',
			'48',
			'56',
			'60',
			'64',
			'80',
			'96',
			'100',
			'128',
			'160',
			'240',
			'256',
			'300',
			'320',
			'480',
			'512',
			'600',
			'640',
			'1024',
			'1280',
			'1536',
			'2048',
			'3072',
			'4096',
		]),
		'128',
	),
	quality: v.optional(v.picklist(['high', 'low', 'lossless']), 'high'),
	animated: v.pipe(
		v.optional(v.picklist(['true', 'false']), 'false'),
		v.transform((v) => v === 'true'),
	),
});

export const ExternalQuerySchema = v.object({
	width: v.optional(v.pipe(v.string(), v.transform(Number), v.number(), v.integer(), v.minValue(1), v.maxValue(4096))),
	height: v.optional(v.pipe(v.string(), v.transform(Number), v.number(), v.integer(), v.minValue(1), v.maxValue(4096))),
	format: v.optional(v.picklist(['png', 'jpg', 'jpeg', 'webp', 'gif'])),
	quality: v.optional(v.picklist(['high', 'low', 'lossless']), 'lossless'),
	animated: v.pipe(
		v.optional(v.picklist(['true', 'false']), 'false'),
		v.transform((v) => v === 'true'),
	),
});
